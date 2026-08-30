#!/usr/bin/env python3
"""Fail-closed static security checks for the CLOG 3 Hypermedia Runtime.

The scanner intentionally examines only the new runtime ownership boundaries,
GitHub Actions workflows, and the optional vendored HTMX manifest. Legacy CLOG
and Builder code are outside HM-004's policy surface.
"""

from __future__ import annotations

import argparse
import hashlib
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


@dataclass(frozen=True)
class Finding:
    rule: str
    path: Path
    line: int
    detail: str


LISP_RULES = (
    (
        "LISP-EVAL",
        re.compile(r"\(\s*(?:cl:)?eval\b", re.IGNORECASE),
        "new runtime code must not evaluate runtime forms",
    ),
    (
        "LISP-READ-FROM-STRING",
        re.compile(r"\(\s*(?:cl:)?read-from-string\b", re.IGNORECASE),
        "external input must not be parsed with READ-FROM-STRING",
    ),
    (
        "LISP-INTERN",
        re.compile(r"\(\s*(?:cl:)?intern\b", re.IGNORECASE),
        "external names must not be interned into the Lisp symbol table",
    ),
    (
        "LEGACY-JS-EXECUTE",
        re.compile(r"(?<![A-Za-z0-9_-])js-execute(?![A-Za-z0-9_-])", re.IGNORECASE),
        "the Hypermedia Runtime must not depend on Legacy raw JavaScript execution",
    ),
)

JS_RULES = (
    (
        "JS-EVAL",
        re.compile(r"(?<![A-Za-z0-9_$])eval\s*\(", re.IGNORECASE),
        "runtime client code must not call eval",
    ),
    (
        "JS-NEW-FUNCTION",
        re.compile(r"\bnew\s+Function\s*\(", re.IGNORECASE),
        "runtime client code must not construct functions from strings",
    ),
    (
        "JS-STRING-TIMEOUT",
        re.compile(r"\bsetTimeout\s*\(\s*['\"`]", re.IGNORECASE),
        "setTimeout must receive a callable, not JavaScript source text",
    ),
)

CDN_RULES = (
    (
        "FRONTEND-CDN",
        re.compile(
            r"https?://(?:unpkg\.com|cdn\.jsdelivr\.net|cdnjs\.cloudflare\.com)",
            re.IGNORECASE,
        ),
        "new runtime code must load frontend assets locally",
    ),
    (
        "FLOATING-FRONTEND",
        re.compile(r"(?:@latest\b|/(?:latest|master)(?:/|\b))", re.IGNORECASE),
        "frontend asset references must use immutable explicit versions",
    ),
)

USES_RE = re.compile(r"^\s*(?:-\s*)?uses\s*:\s*([^\s#]+)", re.IGNORECASE)
PINNED_ACTION_RE = re.compile(r"^[^@\s]+@[0-9a-fA-F]{40}$")


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8", errors="replace")


def line_number(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def scan_regex_rules(path: Path, rules: Iterable[tuple[str, re.Pattern[str], str]]) -> list[Finding]:
    text = read_text(path)
    findings: list[Finding] = []
    for rule, pattern, detail in rules:
        for match in pattern.finditer(text):
            findings.append(Finding(rule, path, line_number(text, match.start()), detail))
    return findings


def iter_files(root: Path, relative: str, suffixes: tuple[str, ...] | None = None) -> Iterable[Path]:
    base = root / relative
    if not base.exists():
        return ()
    if base.is_file():
        if suffixes is None or base.suffix.lower() in suffixes:
            return (base,)
        return ()
    files = []
    for path in sorted(base.rglob("*")):
        if not path.is_file():
            continue
        if suffixes is None or path.suffix.lower() in suffixes:
            files.append(path)
    return tuple(files)


def scan_runtime(root: Path) -> list[Finding]:
    findings: list[Finding] = []

    lisp_files = []
    for relative in ("source/hypermedia", "source/live"):
        lisp_files.extend(iter_files(root, relative, (".lisp",)))
    for path in lisp_files:
        findings.extend(scan_regex_rules(path, LISP_RULES))
        findings.extend(scan_regex_rules(path, CDN_RULES))

    client = root / "static-files/js/clog-effects.js"
    if client.is_file():
        findings.extend(scan_regex_rules(client, JS_RULES))
        findings.extend(scan_regex_rules(client, CDN_RULES))

    # Hypermedia-specific templates are optional in P0 and arrive in later tasks.
    # When present, they are covered automatically without scanning Legacy templates.
    for relative in ("templates/hypermedia", "examples/hypermedia"):
        for path in iter_files(root, relative, (".html", ".htm", ".lisp", ".lt", ".js")):
            findings.extend(scan_regex_rules(path, CDN_RULES))

    return findings


def scan_workflows(root: Path) -> list[Finding]:
    findings: list[Finding] = []
    workflow_dir = root / ".github/workflows"
    if not workflow_dir.is_dir():
        return findings

    for path in sorted(workflow_dir.iterdir()):
        if not path.is_file() or path.suffix.lower() not in (".yml", ".yaml"):
            continue
        text = read_text(path)
        findings.extend(scan_regex_rules(path, CDN_RULES))
        for index, line in enumerate(text.splitlines(), start=1):
            match = USES_RE.match(line)
            if not match:
                continue
            reference = match.group(1).strip("'\"")
            if reference.startswith("./"):
                continue
            if not PINNED_ACTION_RE.fullmatch(reference):
                findings.append(
                    Finding(
                        "GH-ACTION-PIN",
                        path,
                        index,
                        f"GitHub Action must be pinned to a full 40-hex commit SHA: {reference}",
                    )
                )
    return findings


def safe_manifest_path(vendor_root: Path, filename: str) -> Path | None:
    candidate = (vendor_root / filename).resolve()
    try:
        candidate.relative_to(vendor_root.resolve())
    except ValueError:
        return None
    return candidate


def scan_htmx_manifest(root: Path) -> list[Finding]:
    vendor_root = root / "static-files/vendor/htmx/4.0.0"
    manifest = vendor_root / "SHA256SUMS"
    if not manifest.is_file():
        return []

    findings: list[Finding] = []
    lines = read_text(manifest).splitlines()
    entry_re = re.compile(r"^([0-9a-fA-F]{64})\s+\*?(.+)$")
    for index, line in enumerate(lines, start=1):
        if not line.strip():
            continue
        match = entry_re.fullmatch(line.strip())
        if not match:
            findings.append(Finding("HTMX-MANIFEST", manifest, index, "malformed SHA256SUMS entry"))
            continue
        expected, filename = match.groups()
        target = safe_manifest_path(vendor_root, filename)
        if target is None:
            findings.append(Finding("HTMX-MANIFEST", manifest, index, "manifest path escapes vendor directory"))
            continue
        if not target.is_file():
            findings.append(Finding("HTMX-CHECKSUM", manifest, index, f"missing vendored file: {filename}"))
            continue
        actual = hashlib.sha256(target.read_bytes()).hexdigest()
        if actual.lower() != expected.lower():
            findings.append(
                Finding(
                    "HTMX-CHECKSUM",
                    target,
                    1,
                    f"SHA-256 mismatch: expected {expected.lower()}, got {actual}",
                )
            )
    return findings


def display_path(path: Path, root: Path) -> str:
    try:
        return path.resolve().relative_to(root.resolve()).as_posix()
    except ValueError:
        return path.as_posix()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, required=True, help="repository root to scan")
    args = parser.parse_args()
    root = args.root.resolve()

    findings = scan_runtime(root)
    findings.extend(scan_workflows(root))
    findings.extend(scan_htmx_manifest(root))
    findings.sort(key=lambda item: (display_path(item.path, root), item.line, item.rule))

    if findings:
        print(f"CLOG Hypermedia security scan failed with {len(findings)} finding(s):", file=sys.stderr)
        for finding in findings:
            print(
                f"[{finding.rule}] {display_path(finding.path, root)}:{finding.line}: {finding.detail}",
                file=sys.stderr,
            )
        return 1

    print("CLOG Hypermedia security scan passed: no forbidden runtime patterns found.")
    if (root / "static-files/vendor/htmx/4.0.0/SHA256SUMS").is_file():
        print("Vendored HTMX 4.0.0 checksum manifest verified.")
    else:
        print("Vendored HTMX 4.0.0 manifest not present on this branch; checksum check is not applicable.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
