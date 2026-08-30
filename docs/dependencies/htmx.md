# HTMX 4.0.0 vendored dependency

HM-003 pins HTMX to one immutable upstream release for offline, intranet and supply-chain reproducible deployments.

## Provenance

- Upstream repository: https://github.com/bigskysoftware/htmx
- Release tag: `v4.0.0`
- Release page: https://github.com/bigskysoftware/htmx/releases/tag/v4.0.0
- Release artifact: `htmx-4.0.0-dist.zip`
- Release artifact URL: https://github.com/bigskysoftware/htmx/releases/download/v4.0.0/htmx-4.0.0-dist.zip
- Release artifact SHA-256: `858d5fb806ed3003704bc9c9a7fc7aad15213dcfc78c5b78b005fff4211ce57c`
- Release publication date: `2026-08-28`
- Upstream package license: `BSD-0-Clause` / `0BSD`
- LICENSE source: exact upstream `v4.0.0` tag

The four JavaScript files are copied byte-for-byte from the authenticated official release artifact. They are not rebuilt, rewritten or re-minified by CLOG.

## Runtime policy

- Runtime assets live only under `static-files/vendor/htmx/4.0.0/`.
- CDN URLs, `latest`, `master`, beta builds and floating version references are forbidden.
- Any HTMX upgrade requires a separate ADR and pull request with a new versioned directory and checksum manifest.

## Vendored files

| File | SHA-256 |
| --- | --- |
| `htmx.min.js` | `e484d9171a9db30a39c8f16e3d709d4137f3211c659f8e6125816635033d593f` |
| `hx-sse.min.js` | `8a834680c4000a9034d79228872372a92e140c810a075cb6d4a76690dfc13085` |
| `hx-ws.min.js` | `a7c11e4eca05417d6299bb40aaacca01572e44605389fc4d5ef12be408a4d03b` |
| `hx-csp.min.js` | `279f659b9ec8658e3d47230cc545b0bb4e3efa4c2d780454d54db104a5257b7d` |
| `LICENSE` | `d3d2456f76414f2456104660ebd65aff1c04cd7966b942bdabd63f3cdb316a38` |

## Verification

```sh
(cd static-files/vendor/htmx/4.0.0 && sha256sum --check SHA256SUMS)
```

`tests/assets.lisp` independently recomputes the pinned SHA-256 digests, checks provenance, validates JavaScript MIME mapping and rejects runtime CDN references.
