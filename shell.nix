with import <nixpkgs> { };

let
  nnflux-pg-start = pkgs.writeShellScriptBin "nnflux-pg-start" ''
    set -euo pipefail
    pg_ctl -D "$NNFLUX_PGDATA" -l "$NNFLUX_TEST_DIR/pg.log" \
           -o "-p $NNFLUX_PGPORT -k $NNFLUX_PGSOCK -h '''" start
    miniflux &
    pid=$!
    echo "$pid" > "$NNFLUX_TEST_DIR/miniflux.pid"
    echo "nnflux: miniflux running at $NNFLUX_TEST_URL (pid $pid)"
  '';

  nnflux-pg-stop = pkgs.writeShellScriptBin "nnflux-pg-stop" ''
    set -euo pipefail
    if [ -f "$NNFLUX_TEST_DIR/miniflux.pid" ]; then
      kill "$(cat "$NNFLUX_TEST_DIR/miniflux.pid")" 2>/dev/null || true
      rm -f "$NNFLUX_TEST_DIR/miniflux.pid"
    fi
    pg_ctl -D "$NNFLUX_PGDATA" stop
  '';

  # Poke at the local Miniflux instance directly, same auth/base-URL
  # nnflux.el itself uses. Usage: nnflux-api METHOD PATH [json-data]
  #   nnflux-api GET /feeds
  #   nnflux-api POST /feeds '{"feed_url":"https://miniflux.app/feed.xml"}'
  nnflux-api = pkgs.writeShellScriptBin "nnflux-api" ''
    set -euo pipefail
    method="''${1:-GET}"
    path="''${2:-/}"
    data="''${3:-}"
    args=(-s -u "$NNFLUX_TEST_USER:$NNFLUX_TEST_PASSWORD" -X "$method"
          "$NNFLUX_TEST_URL/v1$path" -H "Content-Type: application/json")
    if [ -n "$data" ]; then
      args+=(-d "$data")
    fi
    if command -v jq >/dev/null 2>&1; then
      curl "''${args[@]}" | jq .
    else
      curl "''${args[@]}"
      echo
    fi
  '';
in
pkgs.mkShell {

  nativeBuildInputs = [ pkgs.bashInteractive ];

  buildInputs =
    with pkgs;
    [
      miniflux
      postgresql
    ]
    ++ [
      nnflux-pg-start
      nnflux-pg-stop
      nnflux-api
      jq
    ];
}
