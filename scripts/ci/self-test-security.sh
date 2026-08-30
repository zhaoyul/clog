#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
CHECK="$SCRIPT_DIR/check-security.sh"
FIXTURES="$ROOT/tests/static/fixtures"
TMP_ROOT="$(mktemp -d)"
trap 'rm -rf "$TMP_ROOT"' EXIT

pass_count=0

fail() {
  echo "SELF-TEST FAILURE: $*" >&2
  exit 1
}

expect_reject() {
  local name="$1"
  local fixture="$2"
  local relative_path="$3"
  shift 3
  local case_root="$TMP_ROOT/$name"
  local log="$TMP_ROOT/$name.log"

  mkdir -p "$case_root/$(dirname "$relative_path")"
  cp "$FIXTURES/$fixture" "$case_root/$relative_path"

  if CLOG_SECURITY_ROOT="$case_root" bash "$CHECK" >"$log" 2>&1; then
    cat "$log" >&2
    fail "$name was accepted but must be rejected"
  fi

  local rule
  for rule in "$@"; do
    if ! grep -Fq "[$rule]" "$log"; then
      cat "$log" >&2
      fail "$name did not report expected rule $rule"
    fi
  done

  echo "PASS: $name rejected with expected rule(s): $*"
  pass_count=$((pass_count + 1))
}

expect_accept() {
  local name="$1"
  local fixture="$2"
  local relative_path="$3"
  local case_root="$TMP_ROOT/$name"
  local log="$TMP_ROOT/$name.log"

  mkdir -p "$case_root/$(dirname "$relative_path")"
  cp "$FIXTURES/$fixture" "$case_root/$relative_path"

  if ! CLOG_SECURITY_ROOT="$case_root" bash "$CHECK" >"$log" 2>&1; then
    cat "$log" >&2
    fail "$name was rejected but must be accepted"
  fi

  echo "PASS: $name accepted"
  pass_count=$((pass_count + 1))
}

expect_reject \
  javascript-forbidden \
  forbidden-javascript.js \
  static-files/js/clog-effects.js \
  JS-EVAL JS-NEW-FUNCTION JS-STRING-TIMEOUT

expect_reject \
  lisp-forbidden \
  forbidden-lisp.lisp \
  source/hypermedia/request.lisp \
  LISP-EVAL LISP-READ-FROM-STRING LISP-INTERN LEGACY-JS-EXECUTE

expect_reject \
  frontend-cdn \
  forbidden-cdn.html \
  templates/hypermedia/page.html \
  FRONTEND-CDN FLOATING-FRONTEND

expect_reject \
  floating-action \
  floating-action-workflow.yml \
  .github/workflows/unsafe.yml \
  GH-ACTION-PIN

expect_accept \
  callable-timeout \
  safe-javascript.js \
  static-files/js/clog-effects.js

checksum_root="$TMP_ROOT/checksum-mismatch"
checksum_log="$TMP_ROOT/checksum-mismatch.log"
mkdir -p "$checksum_root/static-files/vendor/htmx/4.0.0"
printf '%s\n' 'tampered' > "$checksum_root/static-files/vendor/htmx/4.0.0/htmx.min.js"
printf '%064d  htmx.min.js\n' 0 > "$checksum_root/static-files/vendor/htmx/4.0.0/SHA256SUMS"
if CLOG_SECURITY_ROOT="$checksum_root" bash "$CHECK" >"$checksum_log" 2>&1; then
  cat "$checksum_log" >&2
  fail "checksum mismatch was accepted but must be rejected"
fi
if ! grep -Fq '[HTMX-CHECKSUM]' "$checksum_log"; then
  cat "$checksum_log" >&2
  fail "checksum mismatch did not report HTMX-CHECKSUM"
fi
echo "PASS: checksum mismatch rejected"
pass_count=$((pass_count + 1))

echo "Security scanner self-tests passed: $pass_count cases."
