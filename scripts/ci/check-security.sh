#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ -n "${CLOG_SECURITY_ROOT:-}" ]]; then
  ROOT="$CLOG_SECURITY_ROOT"
elif ROOT_FROM_GIT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null)"; then
  ROOT="$ROOT_FROM_GIT"
else
  ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
fi

exec python3 "$SCRIPT_DIR/security_scan.py" --root "$ROOT"
