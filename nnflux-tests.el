;;; nnflux-tests.el --- Tests for nnflux -*- lexical-binding: t; -*-

;;; Commentary:

;; Run the unit tests (no network, always safe):
;;
;;   emacs -Q -batch -l ert -l nnflux.el -l nnflux-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; The live tests additionally need a real Miniflux server.  They are
;; skipped automatically when NNFLUX_TEST_URL is unset.  To run them
;; against a server, e.g. a local instance:
;;
;;   NNFLUX_TEST_URL=http://localhost:8080 \
;;   NNFLUX_TEST_TOKEN=your-api-token \
;;   emacs -Q -batch -l ert -l nnflux.el -l nnflux-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Basic Auth instead of a token:
;;
;;   NNFLUX_TEST_URL=https://miniflux.example.com \
;;   NNFLUX_TEST_USER=you NNFLUX_TEST_PASSWORD=secret \
;;   emacs -Q -batch ...
;;
;; The live tests are read-only by default: they list feeds, open the
;; first group, fetch headers and one article body.  They don't push
;; marks (read/unread/star) to the server, so they won't touch real
;; feed state.
;;
;; One test, nnflux-test-live-set-mark-round-trip, is an exception: it
;; verifies that mark-pushing correctly translates local article
;; numbers back to real Miniflux ids (see nnflux-request-set-mark).
;; It's opt-in, additionally gated on NNFLUX_TEST_ALLOW_MUTATION=1,
;; and reverts the one entry it touches -- but only run it against a
;; throwaway test feed, not a real account.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nnflux)

;;; Unit tests -- pure functions, no network

(ert-deftest nnflux-test-clean-title ()
  ;; Kept close to the real title -- case, apostrophes, and punctuation
  ;; other than `:' survive untouched.
  (should (equal (nnflux--clean-title "Emacs News!") "Emacs News!"))
  (should (equal (nnflux--clean-title "Dilip's Log") "Dilip's Log"))
  (should (equal (nnflux--clean-title "  Foo --- Bar  ") "Foo --- Bar"))
  (should (equal (nnflux--clean-title "Löffel") "Löffel"))
  ;; `:' is Gnus's method/group separator, so it gets replaced.
  (should (equal (nnflux--clean-title "Tech: News") "Tech- News")))

(ert-deftest nnflux-test-group-name ()
  (should (equal (nnflux--group-name "Emacs News" 1 nil) "Emacs News"))
  (should (equal (nnflux--group-name "Emacs News" 2 '("Emacs News"))
                  "Emacs News-2")))

(ert-deftest nnflux-test-request-list-quotes-spaced-names ()
  "Regression: a group name with a space must come out Lisp-readable.
`gnus-active-to-gnus-format' parses each `nnflux-request-list' line
with `read', which for an unquoted token stops at the first
whitespace -- an unquoted \"Foo Bar 20 1 n\" reads as group `Foo' then
tries to read `Bar' as the count, erroring with something like
`Wrong type argument: number-or-marker-p, Bar'."
  (nnheader-init-server-buffer)
  (let ((key (intern "1")))
    (cl-letf (((symbol-function 'nnflux--get)
               (lambda (path &optional _params)
                 (cond
                  ((equal path "/feeds")
                   (list (list (cons 'id 1) (cons 'title "Ezra Fenollosa Notes"))))
                  ((equal path "/feeds/counters")
                   (list (cons 'reads (list (cons key 5)))
                         (cons 'unreads (list (cons key 15)))))))))
      (nnflux-request-list "nnflux-tests-spacetest")
      (with-current-buffer nntp-server-buffer
        (goto-char (point-min))
        (let ((group (read (current-buffer)))
              (max (read (current-buffer))))
          (should (equal group "Ezra Fenollosa Notes"))
          (should (equal max 20)))))))

(ert-deftest nnflux-test-request-group-syncs-info-marks ()
  "Regression: entering a group must not show already-read articles as
unread. `gnus-select-newsgroup' calls `gnus-request-group' (passing the
group's info as its 4th arg) and then immediately computes unread
articles from that same info, without ever calling
`nnflux-request-update-info' itself. If `nnflux-request-group' does not
sync INFO's read range from what it just fetched, a fresh INFO (as for
any group not yet touched this session) stays nil and every article --
including already-read ones -- looks unread until a later, separate
refresh happens to see a populated `nnflux-group-entries' table."
  ;; nnflux-group-feed/-entries are `defvoo' variables -- nnoo swaps in a
  ;; fresh per-server copy on `nnflux-open-server' (called via
  ;; `nnflux--use-server' inside `nnflux-request-group'), which would
  ;; silently discard a plain `let'-bound hash table set up before that
  ;; point. Opening the server first, then populating its (now current)
  ;; `nnflux-group-feed', is what makes the seeded data actually visible
  ;; to the code under test.
  (let ((info (gnus-info-make "Example Feed" 1 nil)))
    (nnheader-init-server-buffer)
    (nnflux-open-server "nnflux-tests-marksync")
    (puthash "Example Feed" 1 nnflux-group-feed)
    (cl-letf (((symbol-function 'nnflux--get)
               (lambda (_path &optional _params)
                 ;; Server order is newest-first (desc); nnflux-request-group
                 ;; reverses it, so entry 101 (last here) becomes local
                 ;; article 1, the oldest, and entry 100 becomes article 2.
                 (list (cons 'entries
                             (list '((id . 100) (status . "unread"))
                                   '((id . 101) (status . "read"))))))))
      (should (equal (gnus-info-read info) nil))
      (nnflux-request-group "Example Feed" "nnflux-tests-marksync" t info)
      ;; Entry 101 is "read" and became local article 1 -- INFO's read
      ;; range must include it.
      (should (equal (gnus-info-read info) '(1))))))

(ert-deftest nnflux-test-auth-header-token ()
  (let ((nnflux-token "tok123") nnflux-user nnflux-password)
    (should (equal (nnflux--auth-header) (cons "X-Auth-Token" "tok123")))))

(ert-deftest nnflux-test-auth-header-basic ()
  (let (nnflux-token (nnflux-user "alice") (nnflux-password "secret"))
    (should (equal (nnflux--auth-header)
                    (cons "Authorization"
                          (concat "Basic " (base64-encode-string "alice:secret" t)))))))

(defmacro nnflux-tests--with-auth-source (result &rest body)
  "Run BODY with `auth-source-search' stubbed to return (RESULT), a plist.
Keeps tests from touching a real ~/.authinfo."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'auth-source-search)
              (lambda (&rest _) (list ,result))))
     ,@body))

(ert-deftest nnflux-test-auth-header-auth-source-token ()
  (let (nnflux-token nnflux-user nnflux-password
        (nnflux-address "https://miniflux.example.com"))
    (nnflux-tests--with-auth-source (list :user "api-token" :secret "sekret")
      (should (equal (nnflux--auth-header) (cons "X-Auth-Token" "sekret"))))))

(ert-deftest nnflux-test-auth-header-auth-source-basic ()
  (let (nnflux-token nnflux-user nnflux-password
        (nnflux-address "https://miniflux.example.com"))
    (nnflux-tests--with-auth-source (list :user "alice" :secret "secret")
      (should (equal (nnflux--auth-header)
                      (cons "Authorization"
                            (concat "Basic " (base64-encode-string "alice:secret" t))))))))

(ert-deftest nnflux-test-auth-header-auth-source-secret-function ()
  ;; auth-source often returns the secret wrapped in a closure.
  (let (nnflux-token nnflux-user nnflux-password
        (nnflux-address "https://miniflux.example.com"))
    (nnflux-tests--with-auth-source (list :user "api-token" :secret (lambda () "sekret"))
      (should (equal (nnflux--auth-header) (cons "X-Auth-Token" "sekret"))))))

(ert-deftest nnflux-test-auth-header-prefers-explicit-over-auth-source ()
  (let ((nnflux-token "tok123") nnflux-user nnflux-password
        (nnflux-address "https://miniflux.example.com"))
    (nnflux-tests--with-auth-source (list :user "api-token" :secret "should-not-be-used")
      (should (equal (nnflux--auth-header) (cons "X-Auth-Token" "tok123"))))))

(ert-deftest nnflux-test-auth-header-no-address-no-crash ()
  ;; `nnflux-open-server' calls this to validate config before
  ;; `nnflux-address' is necessarily set -- must not error.
  (let (nnflux-token nnflux-user nnflux-password nnflux-address)
    (should (null (nnflux--auth-header)))))

(ert-deftest nnflux-test-auth-header-none ()
  (let (nnflux-token nnflux-user nnflux-password
        (nnflux-address "https://miniflux.example.com"))
    (nnflux-tests--with-auth-source nil
      (should (null (nnflux--auth-header))))))

(ert-deftest nnflux-test-auth-header-token-priority ()
  (let ((nnflux-token "tok123") (nnflux-user "alice") (nnflux-password "secret"))
    (should (equal (nnflux--auth-header) (cons "X-Auth-Token" "tok123")))))

(ert-deftest nnflux-test-message-id ()
  (let ((nnflux-address "https://miniflux.example.com"))
    (should (equal (nnflux--message-id 42) "<flux-42@miniflux.example.com>"))))

(ert-deftest nnflux-test-nov-line ()
  ;; The leading field is the local article NUMBER (101); the Message-ID
  ;; is built from the entry's own (real, sparse) Miniflux `id' (555) --
  ;; the two are deliberately different to catch them being conflated.
  (let* ((nnflux-address "https://miniflux.example.com")
         (line (nnflux--nov-line
                101 '((id . 555) (title . "Hello") (author . "Alice")
                      (published_at . "2026-01-01T00:00:00Z")))))
    (should (equal line "101\tHello\tAlice\t2026-01-01T00:00:00Z\t<flux-555@miniflux.example.com>\t\t-1\t-1\t"))))

(ert-deftest nnflux-test-nov-line-defaults ()
  ;; Missing title/author should fall back, not error.
  (let* ((nnflux-address "https://miniflux.example.com")
         (line (nnflux--nov-line 5 '((id . 5)))))
    (should (equal line "5\t\tunknown\t\t<flux-5@miniflux.example.com>\t\t-1\t-1\t"))))

(ert-deftest nnflux-test-insert-article-optional-headers ()
  ;; When Miniflux reports feed/category/comments/reading-time data, the
  ;; optional headers appear -- hidden by default, shown with `t' -- since
  ;; none of their names match `gnus-visible-headers'.
  (let ((nnflux-address "https://miniflux.example.com")
        (entry '((id . 9) (feed_id . 3) (title . "Hello") (author . "Alice")
                 (published_at . "2026-01-01T00:00:00Z")
                 (content . "<p>hi there</p>")
                 (url . "https://example.com/hi")
                 (comments_url . "https://example.com/hi/comments")
                 (reading_time . 4)
                 (tags . ("emacs" "gnus"))
                 (enclosures . (((url . "https://example.com/ep.mp3")
                                 (mime_type . "audio/mpeg") (size . 12345))))
                 (feed . ((feed_url . "https://example.com/feed.xml")
                          (site_url . "https://example.com/")
                          (category . ((title . "Blogs"))))))))
    (with-temp-buffer
      (nnflux--insert-article entry "Example Feed")
      (goto-char (point-min))
      (should (search-forward "Archived-At: https://example.com/hi\n" nil t))
      (should (search-forward "X-Feed-URL: https://example.com/feed.xml\n" nil t))
      (should (search-forward "X-Site-URL: https://example.com/\n" nil t))
      (should (search-forward "X-Category: Blogs\n" nil t))
      (should (search-forward "X-Comments-URL: https://example.com/hi/comments\n" nil t))
      (should (search-forward "X-Reading-Time: 4 min\n" nil t))
      (should (search-forward "X-Miniflux-URL: https://miniflux.example.com/feed/3/entry/9\n" nil t))
      (should (search-forward "X-Tags: emacs, gnus\n" nil t))
      (should (search-forward "X-Enclosure-URL: https://example.com/ep.mp3\n" nil t))
      ;; Enclosures also get a clickable body link (nnrss.el's own pattern
      ;; for the same problem) -- headers are outside what "w"
      ;; (gnus-summary-browse-url) ever scans. Link text is the URL's own
      ;; filename, size is human-readable via `file-size-human-readable'.
      (should (search-forward "<p><a href=\"https://example.com/ep.mp3\">ep.mp3</a> (audio/mpeg, 12k)</p>\n" nil t)))))

(ert-deftest nnflux-test-insert-article-omits-missing-optional-headers ()
  ;; No feed/category/comments/reading-time/tags/enclosures data, no url,
  ;; and no feed_id (so no Miniflux web UI link either) -- none of the
  ;; optional headers should appear at all, not as empty stubs.
  (let ((nnflux-address "https://miniflux.example.com")
        (entry '((id . 9) (title . "Hello") (author . "Alice")
                 (published_at . "2026-01-01T00:00:00Z") (content . "<p>hi</p>"))))
    (with-temp-buffer
      (nnflux--insert-article entry "Example Feed")
      (goto-char (point-min))
      (dolist (header '("Archived-At:" "X-Feed-URL:" "X-Site-URL:" "X-Category:"
                         "X-Comments-URL:" "X-Reading-Time:" "X-Miniflux-URL:"
                         "X-Tags:" "X-Enclosure-URL:"))
        (should-not (search-forward header nil t))))))

(ert-deftest nnflux-test-set-range ()
  (should (equal (nnflux--set-range nil '(1 2 3) nil) '((1 . 3))))
  (should (equal (nnflux--set-range '((1 . 3)) nil '(2)) '(1 3))))

(ert-deftest nnflux-test-set-tick-mark ()
  (let ((info (gnus-info-make "test" 1 nil)))
    (nnflux--set-tick-mark info '(5 6) nil)
    (should (equal (cdr (assq 'tick (gnus-info-marks info))) '((5 . 6))))
    (nnflux--set-tick-mark info nil '(5))
    (should (equal (cdr (assq 'tick (gnus-info-marks info))) '(6)))))

;; `nnflux--parse-reply' trusts `url-http-response-status' and
;; `url-http-end-of-headers' -- the buffer-local variables
;; `url-retrieve-synchronously' already populates -- rather than
;; re-parsing raw HTTP text itself, so tests set those directly rather
;; than inserting a literal HTTP response.

(ert-deftest nnflux-test-parse-reply-json ()
  (with-temp-buffer
    (insert "{\"id\": 7, \"title\": \"hi\"}")
    (setq-local url-http-response-status 200)
    (setq-local url-http-end-of-headers (point-min))
    (should (equal (nnflux--parse-reply) '((id . 7) (title . "hi"))))))

(ert-deftest nnflux-test-parse-reply-204 ()
  (with-temp-buffer
    (setq-local url-http-response-status 204)
    (setq-local url-http-end-of-headers (point-min))
    (should (eq (nnflux--parse-reply) t))))

(ert-deftest nnflux-test-parse-reply-error ()
  (with-temp-buffer
    (insert "{\"error_message\": \"nope\"}")
    (setq-local url-http-response-status 404)
    (setq-local url-http-end-of-headers (point-min))
    (should (null (nnflux--parse-reply)))))

(ert-deftest nnflux-test-parse-reply-no-response ()
  ;; Neither variable bound at all -- e.g. the retrieval never
  ;; completed -- must not error.
  (with-temp-buffer
    (should (null (nnflux--parse-reply)))))

;;; Live tests -- need a real Miniflux server, skipped without one

(defun nnflux-tests--live-defs ()
  "Server defs alist built from NNFLUX_TEST_* environment variables."
  (let ((url (getenv "NNFLUX_TEST_URL"))
        (token (getenv "NNFLUX_TEST_TOKEN"))
        (user (getenv "NNFLUX_TEST_USER"))
        (password (getenv "NNFLUX_TEST_PASSWORD")))
    (unless url
      (ert-skip "set NNFLUX_TEST_URL to run live tests against a Miniflux server"))
    (delq nil (list (list 'nnflux-address url)
                     (and token (list 'nnflux-token token))
                     (and user (list 'nnflux-user user))
                     (and password (list 'nnflux-password password))))))

(defmacro nnflux-tests--with-live-server (server-var &rest body)
  "Open a live test server bound to SERVER-VAR, then run BODY.
Sets up `nntp-server-buffer' the way Gnus does before calling into
any backend, since we are calling the backend directly here."
  (declare (indent 1))
  `(progn
     (nnheader-init-server-buffer)
     (let ((,server-var "nnflux-tests-live"))
       (should (nnflux-open-server ,server-var (nnflux-tests--live-defs)))
       ,@body)))

(ert-deftest nnflux-test-live-request-list ()
  (nnflux-tests--with-live-server server
    (should (nnflux-request-list server))
    (should (> (hash-table-count nnflux-group-feed) 0))))

(ert-deftest nnflux-test-live-request-group-and-article ()
  (nnflux-tests--with-live-server server
    (nnflux-request-list server)
    (let ((group (car (hash-table-keys nnflux-group-feed))))
      (unless group
        (ert-skip "no feeds on the test server to read a group from"))
      (should (nnflux-request-group group server))
      (let* ((table (gethash group nnflux-group-entries))
             (numbers (and table (hash-table-keys table))))
        (when numbers
          (should (eq (nnflux-retrieve-headers (list (car numbers)) group server) 'nov))
          (should (> (buffer-size nntp-server-buffer) 0))
          (with-temp-buffer
            (should (nnflux-request-article (car numbers) group server (current-buffer)))
            (should (> (buffer-size) 0))))))))

(ert-deftest nnflux-test-live-dense-numbering ()
  "Article numbers are dense (1..N), oldest to newest, not raw entry ids."
  (nnflux-tests--with-live-server server
    (nnflux-request-list server)
    (let ((group (car (hash-table-keys nnflux-group-feed))))
      (unless group
        (ert-skip "no feeds on the test server to read a group from"))
      (nnflux-request-group group server)
      (let* ((table (gethash group nnflux-group-entries))
             (numbers (sort (hash-table-keys table) #'<)))
        (unless numbers
          (ert-skip "no entries on the test server to number"))
        (should (equal numbers (number-sequence 1 (length numbers))))
        (when (> (length numbers) 1)
          (should (< (alist-get 'id (gethash 1 table))
                     (alist-get 'id (gethash (length numbers) table)))))))))

(ert-deftest nnflux-test-live-request-list-counts ()
  "Check that nnflux-request-list reports the server's real read/unread count.
It must not report a placeholder count."
  (nnflux-tests--with-live-server server
    (nnflux-request-list server)
    (let* ((feed-id (car (hash-table-values nnflux-group-feed)))
           (counters (nnflux--get "/feeds/counters"))
           (key (intern (number-to-string feed-id)))
           (expected (+ (or (alist-get key (alist-get 'reads counters)) 0)
                        (or (alist-get key (alist-get 'unreads counters)) 0))))
      (with-current-buffer nntp-server-buffer
        (goto-char (point-min))
        (should (re-search-forward (format "^\\S-+ %d 1 n$" expected) nil t))))))

;;; Mutating live test -- pushes one read+star mark and reverts it.
;;; Opt-in on top of NNFLUX_TEST_URL, since unlike the rest of the live
;;; tests this one writes to the server (see nnflux--sync-star and
;;; nnflux-request-set-mark: this exercises the local-number -> real-id
;;; translation those depend on).  Run against a throwaway test feed,
;;; not a real account, if you enable this.

(ert-deftest nnflux-test-live-set-mark-round-trip ()
  (unless (getenv "NNFLUX_TEST_ALLOW_MUTATION")
    (ert-skip "set NNFLUX_TEST_ALLOW_MUTATION=1 to run the mark-pushing live test"))
  (nnflux-tests--with-live-server server
    (nnflux-request-list server)
    (let ((group (car (hash-table-keys nnflux-group-feed))))
      (unless group
        (ert-skip "no feeds on the test server to mark"))
      (nnflux-request-group group server)
      (let* ((table (gethash group nnflux-group-entries))
             (numbers (hash-table-keys table)))
        (unless numbers
          (ert-skip "no entries on the test server to mark"))
        (let* ((number (car numbers))
               (real-id (alist-get 'id (gethash number table)))
               (was-status (alist-get 'status (gethash number table)))
               (was-starred (eq (alist-get 'starred (gethash number table)) t)))
          (unwind-protect
              (progn
                (nnflux-request-set-mark group (list (list (list number) 'add '(read tick))) server)
                (let ((entry (nnflux--get (format "/entries/%d" real-id))))
                  (should (equal (alist-get 'status entry) "read"))
                  (should (eq (alist-get 'starred entry) t))))
            ;; revert directly via the API (independent of the code under
            ;; test) since this ran against a real server. The bookmark
            ;; endpoint toggles, so only undo it if it wasn't already starred.
            (nnflux--put "/entries" `((entry_ids . ,(vector real-id))
                                       (status . ,(or was-status "unread"))))
            (unless was-starred
              (nnflux--put (format "/entries/%d/bookmark" real-id) nil))))))))

;;; Search helpers ("G G") -- gated on gnus-search being available,
;;; same as nnflux.el itself; degrade to skipped, not failed, without it.

(ert-deftest nnflux-test-group-for-feed ()
  (skip-unless (featurep 'gnus-search))
  (let ((nnflux-group-feed (make-hash-table :test 'equal)))
    (puthash "Feed A" 1 nnflux-group-feed)
    (puthash "Feed B" 2 nnflux-group-feed)
    (should (equal (nnflux--group-for-feed 2) "Feed B"))
    (should (null (nnflux--group-for-feed 99)))))

(ert-deftest nnflux-test-number-for-real-id ()
  (skip-unless (featurep 'gnus-search))
  (let ((nnflux-group-entries (make-hash-table :test 'equal))
        (table (make-hash-table :test 'eql)))
    (puthash 1 '((id . 500)) table)
    (puthash 2 '((id . 501)) table)
    (puthash "Feed A" table nnflux-group-entries)
    (should (equal (nnflux--number-for-real-id "Feed A" 501) 2))
    (should (null (nnflux--number-for-real-id "Feed A" 999)))
    (should (null (nnflux--number-for-real-id "No Such Group" 500)))))

(provide 'nnflux-tests)

;;; nnflux-tests.el ends here
