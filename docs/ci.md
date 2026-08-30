# CLOG 3 Hypermedia CI and security guardrails

HM-004 establishes the Phase 0 CI floor for the new Hypermedia Runtime. The goal is to make dangerous parsing and JavaScript execution patterns fail before later HTTP, component, SSE, WebSocket, effect or compatibility tasks can merge them.

## Local entry points

The static security policy is not CI-only. Run the exact same entry point used by GitHub Actions:

```sh
bash scripts/ci/check-security.sh
```

Run its malicious-fixture proof suite with:

```sh
bash scripts/ci/self-test-security.sh
```

Validate workflow YAML syntax with Ruby's standard YAML parser:

```sh
bash scripts/ci/check-workflow-syntax.sh .github/workflows/hypermedia-ci.yml
```

`check-security.sh` requires Python 3 and uses only the Python standard library. The workflow syntax helper requires Ruby. GitHub Actions also parses the workflow before it can execute, so malformed Actions YAML cannot become a green workflow run.

## Security policy surface

The scanner deliberately covers the new runtime ownership boundaries rather than Legacy CLOG or CLOG Builder:

- `source/hypermedia/**/*.lisp`
- `source/live/**/*.lisp`
- `static-files/js/clog-effects.js`, when introduced by a later task
- `templates/hypermedia/**` and `examples/hypermedia/**`, when those directories are introduced
- `.github/workflows/*.yml` and `.github/workflows/*.yaml`
- `static-files/vendor/htmx/4.0.0/SHA256SUMS`, when HM-003 assets are present

Legacy files such as `source/clog-base.lisp`, `source/clog-element.lisp`, `source/clog-connection-websockets.lisp`, `static-files/js/boot.js` and `tools/**` are intentionally outside this new-runtime static policy. HM-004 does not change their behavior.

## Fail-closed rules

The static check returns a non-zero status when it finds any of the following:

| Rule | Rejected pattern |
| --- | --- |
| `JS-EVAL` | JavaScript `eval(...)` |
| `JS-NEW-FUNCTION` | JavaScript `new Function(...)` |
| `JS-STRING-TIMEOUT` | string-valued `setTimeout(...)` |
| `LISP-EVAL` | direct Lisp `(eval ...)` |
| `LISP-READ-FROM-STRING` | direct `(read-from-string ...)` in new runtime code |
| `LISP-INTERN` | direct `(intern ...)` in new runtime code |
| `LEGACY-JS-EXECUTE` | dependency on Legacy `js-execute` from the new runtime |
| `FRONTEND-CDN` | unpkg, jsDelivr or cdnjs frontend URLs |
| `FLOATING-FRONTEND` | `latest` or `master` frontend references |
| `GH-ACTION-PIN` | external GitHub Action not pinned to a full 40-hex commit SHA |
| `HTMX-MANIFEST` | malformed or escaping vendored checksum entry |
| `HTMX-CHECKSUM` | missing or checksum-mismatched vendored HTMX file |

There is no inline suppression syntax. A legitimate need to weaken one of these rules changes a frozen security boundary and requires an explicit design/ADR review rather than a local bypass comment.

## Self-test fixtures

`tests/static/fixtures/` contains inputs that must be rejected plus one callable `setTimeout` example that must remain accepted. The self-test copies each fixture into a temporary repository-shaped tree, invokes `bash scripts/ci/check-security.sh` against that tree, and checks the exact rule identifiers returned by the scanner.

This proves the acceptance condition that deliberately introducing `eval(...)` into the new runtime makes the shared local/CI security check fail. The fixture files themselves are not part of the production scan surface.

## GitHub Actions pipeline

`.github/workflows/hypermedia-ci.yml` currently implements the Phase 0 subset of the architecture CI pipeline:

1. **Static security guardrails**
   - workflow YAML syntax check
   - malicious fixture self-tests
   - repository security scan
   - lint log artifact
2. **ASDF compile and FiveAM unit tests**
   - SBCL/Quicklisp dependency setup
   - direct ASDF load of `clog/hypermedia`, `clog/live`, `clog/presentations2` and `clog/compat`
   - `asdf:test-system :clog/hypermedia-tests`
   - compile/test log artifact
3. **CI report gate**
   - aggregates required stage status
   - writes the GitHub step summary
   - uploads a stage report artifact
   - fails if either required stage failed

Clack integration, sample-server browser tests and benchmarks enter the pipeline in the later tasks that actually implement those subsystems. HM-004 does not fabricate empty browser or integration stages.

## Supply-chain policy

GitHub Actions are pinned to immutable commit SHAs rather than floating major tags:

- `actions/checkout` v5.0.0 commit: `08c6903cd8c0fde910a37f88322edcfb5dd907a8`
- `actions/upload-artifact` v5.0.0 commit: `330a01c490aca151604b8cf639adc76d48f6c5d4`

The CI workflow does not download HTMX, JavaScript libraries or other frontend assets from a CDN, `latest`, `master` or another floating frontend reference.

The existing HM-001/HM-002 Common Lisp bootstrap still installs the declared Ultralisp distribution through the repository's inherited HTTP package-manager path. That pre-existing supply-chain limitation is separate from frontend asset delivery and remains documented by the frozen baseline. HM-004 does not widen it.

When the HM-003 versioned HTMX directory is present in an integrated branch, `check-security.sh` automatically recomputes every SHA-256 listed by `static-files/vendor/htmx/4.0.0/SHA256SUMS` and fails closed on malformed entries, missing files or digest drift.

## Artifacts

Successful and failed CI runs retain logs for 14 days:

- `hypermedia-lint-<run-id>`
- `hypermedia-unit-<run-id>`
- `hypermedia-report-<run-id>`

Artifact upload steps use immutable Action commit pins and execute with `if: always()` so diagnostics remain available when a required stage fails.
