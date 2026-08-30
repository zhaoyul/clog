#!/usr/bin/env bash
set -euo pipefail

if ! command -v ruby >/dev/null 2>&1; then
  echo "Ruby is required for the local YAML syntax check." >&2
  exit 2
fi

if [[ "$#" -eq 0 ]]; then
  set -- .github/workflows/*.yml .github/workflows/*.yaml
fi

checked=0
for workflow in "$@"; do
  [[ -e "$workflow" ]] || continue
  ruby -e 'require "yaml"; YAML.parse_file(ARGV.fetch(0)); puts "YAML syntax OK: #{ARGV.fetch(0)}"' "$workflow"
  checked=$((checked + 1))
done

if [[ "$checked" -eq 0 ]]; then
  echo "No workflow files found to validate." >&2
  exit 1
fi
