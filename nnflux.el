;;; nnflux.el --- Read feeds from Miniflux server as a Gnus back end -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dilip

;; Author: Dilip | Zororg
;; Keywords: news, rss, atom, feed, miniflux
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/idlip/nnflux
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; One Gnus group per Miniflux feed.  Read/unread and star state
;; sync both ways with the Miniflux server as you use it in Gnus.
;;
;; Setup can also use auth-source for token/password, leave it empty
;; With an API token (Settings -> API Keys on the server):
;;   (setq gnus-secondary-select-methods
;;         '((nnflux "home"
;;            (nnflux-address "https://miniflux.domain.tld")
;;            (nnflux-token "your-api-token"))))
;;
;; Or with your account username/password instead of a token:
;;   (setq gnus-secondary-select-methods
;;         '((nnflux "home"
;;            (nnflux-address "https://miniflux.domain.tld")
;;            (nnflux-user "your-username")
;;            (nnflux-password "your-password"))))
;;
;; add to ~/.authinfo for password to auto fetch
;; machine miniflux.mydomain.com login token password my-secret-api-token
;;
;; Features and how to use them, all through normal Gnus keys:
;;
;; - Read a feed: enter its group as usual (RET in the group buffer).
;; - Mark read/unread: normal Gnus read/unread commands (e.g. "d" to
;;   mark read, "M-u" to mark unread).  Pushed to the server when you
;;   leave the group (press "q" in the article list) -- Gnus batches
;;   marks and pushes them on group exit, not as you read each article.
;; - Star an article: Gnus's "tick" mark ("!" in the article list)
;;   syncs to Miniflux's star/bookmark, both ways.  "M-u" removes it.
;; - Pick up read/star changes made outside Gnus (e.g. on the
;;   Miniflux web UI or phone app): press "M-g" on the group, or just
;;   re-enter it.  The server is treated as the source of truth for
;;   any article it reports on; local marks for those articles are
;;   overwritten to match it, so there is nothing to resolve by hand.
;; - Podcast/media enclosures show as a clickable link in the article
;;   body (so "w"/gnus-summary-browse-url finds it), and the
;;   X-Enclosure-URL header is made visible by default.
;; - press 't' to see more info for each posts embedded
;; - Use nnvirtual to group all feed into flat list
;;
;;
;;; Code:

(require 'nnoo)
(require 'nnheader)
(require 'gnus)
(require 'range)
(require 'mml)
(require 'url)
(require 'url-http)
(require 'json)
(require 'subr-x)
(require 'auth-source)

;; Tell Gnus this backend can push read/star marks to its server
(gnus-declare-backend "nnflux" 'none 'address 'server-marks)

;; Make the enclosure header visible by default
(defvar gnus-visible-headers) ; defined in gnus-art.el
(with-eval-after-load 'gnus-art
  (setq gnus-visible-headers (concat gnus-visible-headers "\\|^X-Enclosure-URL:")))

(defgroup nnflux nil
  "Read Miniflux feeds through Gnus."
  :group 'gnus)

(defcustom nnflux-fetch-limit 0
  "How many entries to pull per feed when a group is opened.
Sent directly as SQL limit. Keep 0 for no limit."
  :type 'integer
  :group 'nnflux)

(nnoo-declare nnflux)

;; See Info node `(gnus)Select Methods'.

(defcustom nnflux-address nil
  "Base URL of the Miniflux server, e.g. \"https://miniflux.domain.tld\"."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'nnflux)
(nnoo-define 'nnflux-address nil)

(defcustom nnflux-token nil
  "Miniflux API token.  Settings -> API Keys on the server.
Takes priority over `nnflux-user'/`nnflux-password' if both are set."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'nnflux)
(nnoo-define 'nnflux-token nil)

(defcustom nnflux-user nil
  "Miniflux account username, used for Basic Auth when no token is set."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'nnflux)
(nnoo-define 'nnflux-user nil)

(defcustom nnflux-password nil
  "Miniflux account password, used with `nnflux-user' for Basic Auth."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'nnflux)
(nnoo-define 'nnflux-password nil)

(defvoo nnflux-status-string ""
  "Last error message from this server.")

;; auth-source fallback, tried only when neither `nnflux-token' nor
;; `nnflux-user'/`nnflux-password' is set.
;;   machine miniflux.domain.tld login api-token password your-api-token
(defun nnflux--auth-source-host ()
  "Host to look up in auth-source, from `nnflux-address'."
  (and nnflux-address (url-host (url-generic-parse-url nnflux-address))))

(defun nnflux--auth-source-secret (secret)
  "Resolve auth-source SECRET, which may be a function."
  (if (functionp secret) (funcall secret) secret))

(defun nnflux--auth-source-credentials ()
  "(LOGIN . SECRET) from auth-source for `nnflux-address', or nil."
  (let* ((host (nnflux--auth-source-host))
         (match (and host (car (auth-source-search :host host :max 1 :require '(:secret)))))
         (user (plist-get match :user))
         (secret (and match (nnflux--auth-source-secret (plist-get match :secret)))))
    (when (and user secret) (cons user secret))))

(defun nnflux--auth-source-token-login-p (login)
  "Non-nil when auth-source LOGIN denotes an API token, not a username."
  (member (downcase login) '("token" "api-token" "nnflux-token")))

(defvoo nnflux-group-feed (make-hash-table :test 'equal)
  "Map from a group name to its Miniflux feed id.")

;; Value: a hash of local article number -> entry alist, filled on
;; group entry.  Numbers are dense (1..N, oldest to newest of the
;; current fetch) and only meaningful for the session that fetched
;; them; see `nnflux-request-group'.  The real (global, sparse)
;; Miniflux entry id always lives in the entry alist's own `id' field.
(defvoo nnflux-group-entries (make-hash-table :test 'equal)
  "Map from a group name to its article table for this session.")

(nnoo-define-basics nnflux)

;;; Core :: Talking to the Miniflux API

(defun nnflux--parse-reply ()
  "Turn the HTTP reply in the current buffer into a Lisp value.
Returns parsed JSON, t for an empty 204 reply, or nil on error.
Status and body position come from `url-http-response-status'/
`url-http-end-of-headers', which `url-retrieve-synchronously' already
parses into buffer-local variables then no need to re-parse the raw HTTP
text ourselves."
  (let ((status (bound-and-true-p url-http-response-status))
        (body-start (bound-and-true-p url-http-end-of-headers)))
    (cond
     ((not body-start) nil)
     ((or (not status) (>= status 400))
      (nnheader-report 'nnflux "http error %s" (or status "?")))
     ((= status 204) t)
     (t (let ((json-object-type 'alist) (json-array-type 'list))
          (json-read-from-string
           (decode-coding-string
            (buffer-substring-no-properties body-start (point-max)) 'utf-8)))))))

(defun nnflux--basic-auth-header (user password)
  "Basic Auth header for USER/PASSWORD."
  (cons "Authorization"
        (concat "Basic " (base64-encode-string (concat user ":" password) t))))

(defun nnflux--auth-header ()
  "Auth header: API token if set, else user/password, else auth-source."
  (cond
   (nnflux-token (cons "X-Auth-Token" nnflux-token))
   ((and nnflux-user nnflux-password) (nnflux--basic-auth-header nnflux-user nnflux-password))
   (t (let ((credentials (nnflux--auth-source-credentials)))
        (when credentials
          (let ((login (car credentials)) (secret (cdr credentials)))
            (if (nnflux--auth-source-token-login-p login)
                (cons "X-Auth-Token" secret)
              (nnflux--basic-auth-header login secret))))))))

(defun nnflux--request (method path &optional data params)
  "Send METHOD (a string) to PATH, with optional JSON DATA and query PARAMS.
PARAMS is in `url-build-query-string''s form: a list of (key val) lists."
  (let* ((url (concat (string-remove-suffix "/" nnflux-address) "/v1" path))
         (url (if params (concat url "?" (url-build-query-string params)) url))
         (url-request-method method)
         (url-request-extra-headers
          (delq nil (list (nnflux--auth-header)
                          (cons "Content-Type" "application/json"))))
         (url-request-data (and data (encode-coding-string (json-encode data) 'utf-8))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t)
          (prog1 (nnflux--parse-reply)
            (kill-buffer)))
      (error (nnheader-report 'nnflux "%s" (error-message-string err))))))

(defun nnflux--get (path &optional params)
  "Send a GET request to PATH with query PARAMS."
  (nnflux--request "GET" path nil params))

(defun nnflux--put (path data)
  "Send a PUT request to PATH with DATA."
  (nnflux--request "PUT" path data))

(defun nnflux--post (path data)
  "Send a POST request to PATH with DATA."
  (nnflux--request "POST" path data))

;;; Turning a feed title into a group name

(defun nnflux--clean-title (title)
  "TITLE as a Gnus group name, kept close to its real form.
Only `:' (Gnus's method/group separator) and control characters are
touched; case, spaces, and punctuation are otherwise left alone."
  (string-trim (replace-regexp-in-string
                "[[:cntrl:]]" " " (replace-regexp-in-string ":" "-" title))))

(defun nnflux--group-name (title id used)
  "Group name for feed TITLE/ID, adding ID if it collides with one in USED."
  (let ((name (nnflux--clean-title title)))
    (if (member name used) (format "%s-%d" name id) name)))

;;; Required Gnus backend interface

(deffoo nnflux-open-server (server &optional defs)
  "Open SERVER and check that a connection setting exists."
  (nnoo-change-server 'nnflux server defs)
  (if (and nnflux-address (nnflux--auth-header))
      t
    (nnheader-report
     'nnflux "set nnflux-address, and nnflux-token, nnflux-user/nnflux-password, or an auth-source entry")))

(defun nnflux--server-defs (server)
  "Miniflux SERVER address (VAR VALUE) from Gnus's select-method config."
  (catch 'found
    (dolist (method (cons gnus-select-method gnus-secondary-select-methods))
      (when (and (eq (car method) 'nnflux) (equal (cadr method) server))
        (throw 'found (cddr method))))
    nil))

(defun nnflux--use-server (server)
  "Make sure SERVER's connection settings are active."
  (when (and server (not (nnflux-server-opened server)))
    (nnflux-open-server server (nnflux--server-defs server))))

(deffoo nnflux-request-list (&optional server)
  "List every feed on the server as a Gnus group."
  (nnflux--use-server server)
  (let* ((feeds (nnflux--get "/feeds"))
         ;; One cheap call for every feed's read/unread totals, so the
         ;; active range below is a real count instead of a placeholder.
         (counters (nnflux--get "/feeds/counters"))
         (reads (alist-get 'reads counters))
         (unreads (alist-get 'unreads counters))
         used)
    (clrhash nnflux-group-feed)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (feed feeds)
        (let* ((id (alist-get 'id feed))
               (title (or (alist-get 'title feed) (format "feed-%d" id)))
               (group (nnflux--group-name title id used))
               (key (intern (number-to-string id)))
               (total (+ (or (alist-get key reads) 0) (or (alist-get key unreads) 0))))
          (push group used)
          (puthash group id nnflux-group-feed)
          ;; %S, not %s: Gnus parses this line with `(read cur)', which
          ;; reads an unquoted token only up to the first whitespace --
          ;; group names can contain spaces since nnflux--clean-title
          ;; stopped stripping them, so the name must be Lisp-readable.
          (insert (format "%S %d 1 n\n" group total)))))
    t))

(deffoo nnflux-request-group (group &optional server _fast info)
  "Fetch GROUP's entries from the server and report its article range.
Articles are numbered 1..N, oldest to newest of this fetch -- not by
their real Miniflux id, which is sparse and global across every feed
on the server and would otherwise throw off Gnus's unread-count math.
The real id lives in the entry alist and is used for all API calls."
  (nnflux--use-server server)
  (let ((feed-id (gethash group nnflux-group-feed)))
    (if (not feed-id)
        (nnheader-report 'nnflux "no such group: %s" group)
      (let* ((data (nnflux--get (format "/feeds/%d/entries" feed-id)
                                  `(("limit" ,nnflux-fetch-limit)
                                    ("order" "id") ("direction" "desc"))))
             ;; Server gives newest-first (desc, so the fetch-limit window
             ;; is the most recent N); reverse to number oldest-to-newest.
             (entries (reverse (alist-get 'entries data)))
             (table (make-hash-table :test 'eql))
             (n 0))
        (dolist (entry entries)
          (setq n (1+ n))
          (puthash n entry table))
        (puthash group table nnflux-group-entries)
        (when info (nnflux-request-update-info group info server))
        (nnheader-insert "211 %d %d %d %s\n" n (if (> n 0) 1 0) n group)))))

(deffoo nnflux-close-group (_group &optional _server)
  "Close a group. This backend needs no cleanup."
  t)

(deffoo nnflux-request-post (&optional _server)
  "Report that Miniflux does not support posting."
  (nnheader-report 'nnflux "posting is not supported"))

;; helper to get small description

(defun nnflux--describe-feed (feed)
  "Return a one-line description for FEED, a Miniflux feed JSON object.
Use its description if it has one, else its site url.  Flatten any
newlines in the result."
  (let ((description (alist-get 'description feed))
        (site (alist-get 'site_url feed)))
    (replace-regexp-in-string
     "[ \t\n\r]+" " "
     (string-trim
      (if (and description (not (string-empty-p description)))
          (format "%s (%s)" description (or site ""))
        (or site ""))))))

(deffoo nnflux-request-group-description (group &optional server)
  "Report GROUP's feed description, shown by `gnus-group-describe-group'."
  (nnflux--use-server server)
  (let ((feed-id (gethash group nnflux-group-feed)))
    (when feed-id
      (let ((feed (nnflux--get (format "/feeds/%d" feed-id))))
        (when feed
          (with-current-buffer nntp-server-buffer
            (erase-buffer)
            (insert (format "%d %s\n" feed-id (nnflux--describe-feed feed))))
          t)))))

;;; Pulling read/star state from the server (the server is the source
;;; of truth: whatever it says about an article we just fetched wins
;;; over whatever Gnus had marked locally for it).
;;; Defeats the offline usage, and is primarily focused on online only

(defun nnflux--set-range (old-range add-ids drop-ids)
  "OLD-RANGE with DROP-IDS removed and ADD-IDS added."
  (range-add-list (range-remove old-range (range-compress-list drop-ids))
                  add-ids))

(defun nnflux--set-tick-mark (info add-ids drop-ids)
  "Update INFO's tick (star) mark range: add ADD-IDS, drop DROP-IDS."
  (let* ((marks (gnus-info-marks info))
         (new (nnflux--set-range (cdr (assq 'tick marks)) add-ids drop-ids)))
    (setf (gnus-info-marks info) (cons (cons 'tick new) (assq-delete-all 'tick marks)))))

(deffoo nnflux-request-update-info (group info &optional server)
  "Make GROUP's local read/star marks in INFO match the server.
Only the entries just fetched by `nnflux-request-group' are touched;
older entries outside that window keep whatever Gnus had for them."
  (nnflux--use-server server)
  (let ((table (gethash group nnflux-group-entries))
        read-ids unread-ids star-ids unstar-ids)
    (when table
      (maphash (lambda (number entry)
                 (if (equal (alist-get 'status entry) "read")
                     (push number read-ids)
                   (push number unread-ids))
                 (if (eq (alist-get 'starred entry) t)
                     (push number star-ids)
                   (push number unstar-ids)))
               table)
      (setf (gnus-info-read info)
            (nnflux--set-range (gnus-info-read info) read-ids unread-ids))
      (nnflux--set-tick-mark info star-ids unstar-ids)
      t)))

;;; Article headers and bodies

(defun nnflux--message-id (id)
  "Message-ID string for entry ID."
  (format "<flux-%d@%s>" id (or (url-host (url-generic-parse-url nnflux-address))
                                 "miniflux")))

(defun nnflux--nov-line (number entry)
  "One tab-separated summary line for local article NUMBER/ENTRY."
  (mapconcat (lambda (x) (format "%s" x))
             (list number
                   (or (alist-get 'title entry) "")
                   (or (alist-get 'author entry) "unknown")
                   (or (alist-get 'published_at entry) "")
                   (nnflux--message-id (alist-get 'id entry))
                   "" -1 -1 "")
             "\t"))

(deffoo nnflux-retrieve-headers (articles &optional group server _fetch-old)
  "Insert one summary line per article, the data Gnus shows in a group."
  (nnflux--use-server server)
  (let ((table (gethash group nnflux-group-entries)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (id articles)
        (when (numberp id)
          (let ((entry (and table (gethash id table))))
            (when entry (insert (nnflux--nov-line id entry) "\n")))))))
  'nov)

(defun nnflux--insert-header (name value)
  "Insert a NAME header with VALUE, unless VALUE is nil or empty.
Used for the optional headers below: since none of these names match
`gnus-visible-headers', Gnus hides them by default and reveals them
with `gnus-summary-toggle-header' (\"t\") like any other header."
  (when (and value (not (string-empty-p value)))
    (insert name ": " value "\n")))

(defun nnflux--miniflux-url (entry)
  "Link to ENTRY's own page in the Miniflux web UI, or nil if unknown.
Uses `/feed/{feedID}/entry/{entryID}' (`showFeedEntryPage' in
Miniflux's own `internal/ui/ui.go' route table) rather than one of the
`/unread/...'/`/starred/...' variants, since those are scoped to a
read-status list this entry may no longer belong to by the time the
link is followed."
  (let ((feed-id (alist-get 'feed_id entry))
        (id (alist-get 'id entry)))
    (when (and feed-id id)
      (format "%s/feed/%d/entry/%d" (string-remove-suffix "/" nnflux-address) feed-id id))))

(defun nnflux--insert-article (entry group)
  "Write ENTRY as a plain message for GROUP into the current buffer.
Give the message an HTML body, plus optional headers.
Hidden by default, shown with key t in article buffer."
  (insert "Newsgroups: " group "\n")
  (insert "Subject: " (or (alist-get 'title entry) "") "\n")
  (insert "From: " (or (alist-get 'author entry) "unknown") "\n")
  (insert "Date: " (or (alist-get 'published_at entry) "") "\n\n")
  (insert "<#part type=\"text/html\">\n<html><body>\n")
  (insert (or (alist-get 'content entry) ""))
  (insert "\n<p><a href=\"" (or (alist-get 'url entry) "") "\">original</a></p>\n")
  ;; Enclosures (podcast/media attachments) as clickable body links, same
  ;; place `nnrss.el' puts them for the identical problem.
  (dolist (enclosure (alist-get 'enclosures entry))
    (let ((enclosure-url (alist-get 'url enclosure)))
      (when (and enclosure-url (not (string-empty-p enclosure-url)))
        (insert "<p><a href=\"" enclosure-url "\">"
                (if (string-match "/\\([^/]*\\)\\'" enclosure-url)
                    (match-string 1 enclosure-url)
                  "enclosure")
                "</a> ("
                (or (alist-get 'mime_type enclosure) "")
                (if (numberp (alist-get 'size enclosure))
                    (format ", %s" (file-size-human-readable (alist-get 'size enclosure)))
                  "")
                ")</p>\n"))))
  (insert "</body></html>\n<#/part>\n")
  (mml-to-mime)
  (goto-char (point-min))
  (search-forward "\n\n")
  (forward-line -1)
  (insert "Message-ID: " (nnflux--message-id (alist-get 'id entry)) "\n")
  (let* ((feed (alist-get 'feed entry))
         (category (and feed (alist-get 'category feed)))
         (reading-time (alist-get 'reading_time entry))
         (tags (delq nil (mapcar (lambda (tag) (and (stringp tag) (not (string-empty-p tag)) tag))
                                  (alist-get 'tags entry))))
         (enclosure-urls (delq nil (mapcar (lambda (enc) (let ((u (alist-get 'url enc)))
                                                            (and u (not (string-empty-p u)) u)))
                                            (alist-get 'enclosures entry)))))
    (nnflux--insert-header "Archived-At" (alist-get 'url entry))
    (nnflux--insert-header "X-Feed-URL" (and feed (alist-get 'feed_url feed)))
    (nnflux--insert-header "X-Site-URL" (and feed (alist-get 'site_url feed)))
    (nnflux--insert-header "X-Category" (and category (alist-get 'title category)))
    (nnflux--insert-header "X-Comments-URL" (alist-get 'comments_url entry))
    (when (and (numberp reading-time) (> reading-time 0))
      (nnflux--insert-header "X-Reading-Time" (format "%d min" reading-time)))
    (nnflux--insert-header "X-Miniflux-URL" (nnflux--miniflux-url entry))
    (when tags (nnflux--insert-header "X-Tags" (mapconcat #'identity tags ", ")))
    (when enclosure-urls
      (nnflux--insert-header "X-Enclosure-URL" (mapconcat #'identity enclosure-urls ", ")))))

(deffoo nnflux-request-article (article &optional group server to-buffer)
  "Fetch ARTICLE (a local article number, see `nnflux-request-group')."
  (nnflux--use-server server)
  (let* ((table (gethash group nnflux-group-entries))
         (entry (and table (gethash article table))))
    (if (not entry)
        (nnheader-report 'nnflux "no such article: %s" article)
      (with-current-buffer (or to-buffer nntp-server-buffer)
        (erase-buffer)
        (nnflux--insert-article entry group)
        (goto-char (point-min)))
      (cons group article))))

;;; Pushing read/unread and star changes back to the server

(defun nnflux--sync-star (group numbers starred)
  "Set the bookmark (star) flag on GROUP's local article NUMBERS to STARRED.
Change only entries where the flag differs from STARRED already."
  (let ((table (gethash group nnflux-group-entries)))
    (dolist (number numbers)
      (let* ((entry (and table (gethash number table)))
             (id (and entry (alist-get 'id entry)))
             (was (and entry (eq (alist-get 'starred entry) t))))
        (when (and id (not (eq was starred)))
          (nnflux--put (format "/entries/%d/bookmark" id) nil)
          (setf (alist-get 'starred entry) starred))))))

(deffoo nnflux-request-set-mark (group actions &optional server)
  "Push read/unread and star changes for GROUP's ACTIONS to the server.
ACTIONS carry local article numbers (see `nnflux-request-group'); each
is translated back to its real Miniflux entry id before hitting the API."
  (nnflux--use-server server)
  (let ((table (gethash group nnflux-group-entries)))
    (dolist (action actions)
      (let* ((numbers (range-uncompress (nth 0 action)))
             (adding (eq (nth 1 action) 'add))
             (marks (nth 2 action))
             (ids (delq nil (mapcar (lambda (n)
                                       (let ((entry (and table (gethash n table))))
                                         (and entry (alist-get 'id entry))))
                                     numbers))))
        (when (and ids (memq 'read marks))
          (nnflux--put "/entries"
                        `((entry_ids . ,(vconcat ids))
                          (status . ,(if adding "read" "unread")))))
        (when (memq 'tick marks)
          (nnflux--sync-star group numbers adding)))))
  nil)

;;; Helper to subscribe a new feed from Gnus, sent to minifeed

(defun nnflux--configured-servers ()
  "Return the server name of every nnflux method configured in Gnus.
Look in both `gnus-select-method' and `gnus-secondary-select-methods'."
  (delq nil (mapcar (lambda (m) (and (eq (car m) 'nnflux) (cadr m)))
                    (cons gnus-select-method gnus-secondary-select-methods))))

(defun nnflux--pick-server ()
  "Pick an nnflux server to use.
Prefer the current group's server.  Else use the sole configured
server, or ask the user when there is more than one."
  (or (and (eq major-mode 'gnus-group-mode)
           (let* ((group (gnus-group-group-name))
                  (method (and group (gnus-find-method-for-group group))))
             (and method (eq (car method) 'nnflux) (cadr method))))
      (let ((servers (nnflux--configured-servers)))
        (cond ((null servers) (user-error "No nnflux server configured"))
              ((null (cdr servers)) (car servers))
              (t (completing-read "Nnflux server: " servers nil t))))))

;;;###autoload
(defun nnflux-subscribe-feed (url &optional server)
  "Subscribe to the feed at URL on SERVER, then check for new groups.
SERVER defaults to the current group's server in the Group buffer, the
sole configured nnflux server, or a prompt if there's more than one.
Newly subscribed feeds are picked up by Gnus's own new-group handling
\(the same as pressing \\<gnus-group-mode-map>\\[gnus-group-get-new-news]\), not a custom refresh here."
  (interactive (list (read-string "Feed URL: " (url-get-url-at-point)) (nnflux--pick-server)))
  (nnflux--use-server server)
  (let* ((result (nnflux--post "/feeds" `((feed_url . ,url))))
         (feed-id (alist-get 'feed_id result)))
    (if (not feed-id)
        (message "nnflux: failed to subscribe to %s" url)
      (nnflux--put (format "/feeds/%d/refresh" feed-id) nil)
      (message "nnflux: subscribed to %s, checking for new groups..." url)
      (gnus-group-get-new-news))))

(provide 'nnflux)

;;; nnflux.el ends here
