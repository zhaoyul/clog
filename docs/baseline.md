# CLOG 3 Hypermedia Runtime baseline

This document freezes the repository and dependency baseline used to begin the
CLOG 3 Hypermedia Runtime work. The source baseline is anchored only by the
full commit SHA below. Branch names are recorded as context and are not used as
version pins.

## Source baseline

| Field | Frozen value |
|---|---|
| Repository | `zhaoyul/clog` |
| Upstream repository | `rabbibotton/clog` |
| Branch context | `main` |
| Baseline commit | `9ee022a97ab4109f2ff6d2b9c43ba6da286d6302` |
| Baseline commit date | `2026-06-05T16:52:11Z` |
| Baseline tree | `996f700d7d6a4bd9913f9c8ce41a55015d3f118e` |
| CLOG ASDF version | `2.2` |
| Baseline `clog.asd` blob | `49191a631f5ee7104892686afdfa44e63372c459` |
| Baseline `qlfile` blob | `4c107cb03ffadcc40e81e6f5c79cb4b12aa706b8` |

The implementation branch descends directly from the baseline commit above.
The existing `#:clog` ASDF component order is frozen by the
`baseline/asdf/legacy-component-order-is-frozen` regression test.

## Dependency baseline

The baseline repository contains a `qlfile` with exactly one distribution
entry:

```text
dist http://dist.ultralisp.org/
```

No `qlfile.lock`, OCICL lock file, or vendored Common Lisp dependency tree is
present at the frozen commit. The repository-level dependency lock state is
therefore **absent**. HM-001 records the concrete versions observed during clean
verification rather than claiming that the baseline already had a lock file.

The unencrypted `http` distribution URL is inherited from the frozen Legacy
baseline and is not changed by HM-001. Pinning dependency archives and moving
the distribution endpoint to authenticated HTTPS are separate supply-chain
hardening tasks.

## Verified toolchain

Clean verification used GitHub Actions run `33315459166` on verification
commit `ebb9fc98cda98f8379581c61ec899d819adee1e0`. The temporary verification
workflow is not part of the final HM-001 change set. The final squashed commit
preserves the verified test blobs and executable ASDF forms. After the run, one
pre-existing Legacy line's trailing spaces were restored and this document was
finalized; neither change affects the verified Lisp behavior.

The concrete toolchain was:

| Field | Verified value |
|---|---|
| Runner image | `ubuntu-24.04` |
| Operating system | `Ubuntu 24.04.4 LTS` |
| Runner image version | `20260823.283.1` |
| SBCL | `2.2.9.debian` |
| ASDF loaded version | `3.3.1` |
| Quicklisp client | `2021-02-13` |
| Quicklisp distribution | `2026-01-01` |
| Ultralisp distribution | `20260829162500` |
| Quicklisp bootstrap SHA-256 | `4a7a5c2aebe0716417047854267397e24a44d0cce096127411e9ce9ccfeb2c17` |

The verification workflow first resolved dependencies through Quicklisp, then
started a fresh SBCL process and loaded `#:clog` directly with ASDF. It also
loaded the test system without starting CLOG or creating a background thread,
ran all baseline tests, and confirmed that an intentional failure exits a
non-interactive Lisp process with a non-zero status.

## Repeatable test entry point

After loading the dependency environment and registering this checkout with
ASDF, run:

```lisp
(asdf:test-system :clog/hypermedia-tests)
```

The clean verification result is 7 FiveAM tests, 44 checks, 44 passes, 0 skips,
and 0 failures.

From a shell, an unhandled test failure exits the non-interactive Lisp process
with a non-zero status. In an interactive REPL, the runner signals
`CLOG-HYPERMEDIA-TESTS::HYPERMEDIA-TEST-FAILURE`, preserving the debugger and
normal Common Lisp restart workflow instead of calling `uiop:quit`.

## Scope of the baseline suite

HM-001 intentionally adds no production Hypermedia API. The suite verifies:

1. `#:clog` loads as a dependency of the test system.
2. The Legacy `#:clog` ASDF component order remains unchanged.
3. Core exported symbols, callables, generic functions, and classes remain
   available.
4. A minimal Legacy CLOG object and on-new-window handler can be constructed
   without starting a server, opening a socket, or creating a background
   thread.
5. Empty suites and failed checks become real Common Lisp error conditions.

## Known Legacy limitations and compatibility boundary

HM-001 does not change browser behavior, the WebSocket transport, static boot
behavior, CLOG Builder, or any production runtime source file. Those behaviors
remain outside the scope of this baseline-only task.

The clean direct-ASDF load emits pre-existing warnings from Legacy CLOG and
third-party dependencies, including generic-function redefinition warnings and
some deprecated CFFI SQLite struct references. The process loads successfully,
all 44 baseline checks pass, CLOG remains stopped, and no new background thread
is created. The warnings are recorded as baseline observations rather than
silenced or modified in HM-001.
