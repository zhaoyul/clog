#!/usr/bin/env python3
"""HM-027 real-browser acceptance using ChromeDriver's WebDriver HTTP API.

The script intentionally depends only on Python's standard library plus an
installed Chrome/Chromium and matching ChromeDriver. It exercises JavaScript-on
HTMX morphing, independent browser sessions and JavaScript-off PRG behavior.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import socket
import subprocess
import sys
import time
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import quote, urljoin, urlparse
from urllib.request import Request, urlopen

ELEMENT_KEY = "element-6066-11e4-a52e-4f735466cecf"


class WebDriverFailure(RuntimeError):
    pass


def find_executable(explicit: str | None, candidates: list[str], common: list[str]) -> str:
    if explicit:
        path = Path(explicit)
        if path.is_dir():
            for candidate in candidates:
                nested = path / candidate
                if nested.is_file() and os.access(nested, os.X_OK):
                    return str(nested)
        if path.is_file() and os.access(path, os.X_OK):
            return str(path)
        raise WebDriverFailure(f"Executable not found: {explicit}")
    for candidate in candidates:
        resolved = shutil.which(candidate)
        if resolved:
            return resolved
    for value in common:
        path = Path(value)
        if path.is_file() and os.access(path, os.X_OK):
            return str(path)
    raise WebDriverFailure(f"Could not find any of: {', '.join(candidates)}")


def free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return int(sock.getsockname()[1])


class DriverServer:
    def __init__(self, driver_path: str, browser_path: str):
        self.driver_path = driver_path
        self.browser_path = browser_path
        self.port = free_port()
        self.base = f"http://127.0.0.1:{self.port}"
        self.process: subprocess.Popen[bytes] | None = None

    def __enter__(self) -> "DriverServer":
        self.process = subprocess.Popen(
            [self.driver_path, f"--port={self.port}"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
        deadline = time.monotonic() + 10
        last_error: Exception | None = None
        while time.monotonic() < deadline:
            if self.process.poll() is not None:
                raise WebDriverFailure(f"ChromeDriver exited early with {self.process.returncode}")
            try:
                status = self.request("GET", "/status")
                if isinstance(status, dict) and status.get("ready") is not False:
                    return self
            except Exception as exc:
                last_error = exc
            time.sleep(0.1)
        raise WebDriverFailure(f"ChromeDriver did not become ready: {last_error}")

    def __exit__(self, exc_type, exc, tb) -> None:
        if self.process and self.process.poll() is None:
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=5)

    def request(self, method: str, path: str, payload=None):
        data = None if payload is None else json.dumps(payload).encode("utf-8")
        request = Request(
            self.base + path,
            data=data,
            method=method,
            headers={"Content-Type": "application/json; charset=utf-8"},
        )
        try:
            with urlopen(request, timeout=20) as response:
                raw = response.read()
        except HTTPError as exc:
            raw = exc.read()
            try:
                body = json.loads(raw.decode("utf-8"))
            except Exception:
                body = raw.decode("utf-8", "replace")
            raise WebDriverFailure(f"WebDriver HTTP {exc.code} for {path}: {body}") from exc
        except URLError as exc:
            raise WebDriverFailure(f"WebDriver request failed for {path}: {exc}") from exc
        body = json.loads(raw.decode("utf-8")) if raw else {"value": None}
        value = body.get("value")
        if isinstance(value, dict) and value.get("error"):
            raise WebDriverFailure(f"WebDriver error for {path}: {value}")
        return value

    def session(self, javascript_enabled: bool = True) -> "Session":
        options = {
            "binary": self.browser_path,
            "args": [
                "--headless=new",
                "--no-sandbox",
                "--disable-dev-shm-usage",
                "--disable-background-networking",
                "--window-size=1280,900",
            ],
        }
        if not javascript_enabled:
            options["prefs"] = {
                "profile.managed_default_content_settings.javascript": 2,
                "profile.default_content_setting_values.javascript": 2,
            }
        capabilities = {
            "browserName": "chrome",
            "pageLoadStrategy": "normal",
            "goog:loggingPrefs": {"performance": "ALL"},
            "goog:chromeOptions": options,
        }
        value = self.request(
            "POST",
            "/session",
            {"capabilities": {"alwaysMatch": capabilities}},
        )
        if not isinstance(value, dict) or not value.get("sessionId"):
            raise WebDriverFailure(f"ChromeDriver returned no session id: {value!r}")
        return Session(self, value["sessionId"])


class Session:
    def __init__(self, server: DriverServer, session_id: str):
        self.server = server
        self.session_id = session_id
        self.prefix = f"/session/{quote(session_id, safe='')}"
        self.closed = False

    def request(self, method: str, suffix: str, payload=None):
        return self.server.request(method, self.prefix + suffix, payload)

    def close(self) -> None:
        if not self.closed:
            try:
                self.request("DELETE", "")
            finally:
                self.closed = True

    def navigate(self, url: str) -> None:
        self.request("POST", "/url", {"url": url})

    def refresh(self) -> None:
        self.request("POST", "/refresh", {})

    def current_url(self) -> str:
        return str(self.request("GET", "/url"))

    def find(self, selector: str, using: str = "css selector") -> str:
        value = self.request("POST", "/element", {"using": using, "value": selector})
        return str(value[ELEMENT_KEY])

    def find_all(self, selector: str, using: str = "css selector") -> list[str]:
        values = self.request("POST", "/elements", {"using": using, "value": selector})
        return [str(value[ELEMENT_KEY]) for value in values]

    def text(self, element_id: str) -> str:
        return str(self.request("GET", f"/element/{quote(element_id, safe='')}/text"))

    def attribute(self, element_id: str, name: str):
        return self.request(
            "GET",
            f"/element/{quote(element_id, safe='')}/attribute/{quote(name, safe='')}",
        )

    def click(self, element_id: str) -> None:
        self.request("POST", f"/element/{quote(element_id, safe='')}/click", {})

    def click_button(self, label: str) -> None:
        xpath = f"//button[normalize-space()={json.dumps(label)}]"
        self.click(self.find(xpath, using="xpath"))

    def wait_text(self, selector: str, expected: str, timeout: float = 8.0) -> None:
        deadline = time.monotonic() + timeout
        last = None
        while time.monotonic() < deadline:
            try:
                last = self.text(self.find(selector))
                if last == expected:
                    return
            except WebDriverFailure:
                pass
            time.sleep(0.05)
        raise AssertionError(f"Expected {selector} text {expected!r}, last value {last!r}")

    def performance_logs(self) -> list[dict]:
        try:
            entries = self.request("POST", "/se/log", {"type": "performance"})
        except WebDriverFailure:
            entries = self.request("POST", "/log", {"type": "performance"})
        messages = []
        for entry in entries or []:
            try:
                outer = json.loads(entry["message"])
                message = outer.get("message", outer)
                if isinstance(message, dict):
                    messages.append(message)
            except (KeyError, TypeError, ValueError):
                continue
        return messages


def assert_counter(session: Session, expected: str) -> None:
    session.wait_text("#counter-value", expected)


def open_counter(session: Session, base_url: str) -> None:
    session.navigate(f"{base_url}/counter")
    assert_counter(session, "0")


def assert_local_htmx(session: Session, base_url: str) -> None:
    scripts = session.find_all('script[src*="/vendor/htmx/4.0.0/htmx.min.js"]')
    assert len(scripts) == 1, f"Expected one local HTMX script, found {len(scripts)}"
    src = session.attribute(scripts[0], "src")
    assert src, "HTMX script has no src"
    resolved = urljoin(base_url + "/", str(src))
    assert urlparse(resolved).netloc == urlparse(base_url).netloc, (
        f"HTMX must be same-origin, got {resolved!r}"
    )


def assert_csp_authorized_htmx_forms(session: Session) -> None:
    forms = session.find_all("form[hx-post][hx-target][hx-swap][hx-nonce]")
    assert len(forms) == 3, f"Expected 3 CSP-authorized HTMX forms, found {len(forms)}"
    for form in forms:
        assert session.attribute(form, "hx-nonce"), "HTMX form is missing hx-nonce"
        assert session.attribute(form, "hx-swap") == "outerMorph"


def action_requests(messages: list[dict]) -> list[dict]:
    results = []
    for message in messages:
        if message.get("method") != "Network.requestWillBeSent":
            continue
        request = message.get("params", {}).get("request", {})
        if "/_clog/action/" in str(request.get("url", "")):
            results.append(request)
    return results


def assert_hx_request(messages: list[dict]) -> None:
    requests = action_requests(messages)
    assert requests, "No action request was observed"
    for request in requests:
        headers = {str(k).lower(): str(v).lower() for k, v in request.get("headers", {}).items()}
        if headers.get("hx-request") == "true":
            return
    raise AssertionError("Action request did not carry HX-Request: true")


def assert_no_hx_request(messages: list[dict]) -> None:
    requests = action_requests(messages)
    assert requests, "No ordinary form action request was observed"
    for request in requests:
        headers = {str(k).lower(): str(v).lower() for k, v in request.get("headers", {}).items()}
        assert headers.get("hx-request") != "true", "JavaScript-off form unexpectedly used HTMX"


def assert_prg_redirect(messages: list[dict]) -> None:
    for message in messages:
        if message.get("method") != "Network.requestWillBeSent":
            continue
        redirect = message.get("params", {}).get("redirectResponse")
        if not isinstance(redirect, dict):
            continue
        if "/_clog/action/" in str(redirect.get("url", "")) and int(redirect.get("status", 0)) == 303:
            return
    raise AssertionError("JavaScript-off mutation did not expose a 303 Post/Redirect/Get transition")


def exercise_js_on(server: DriverServer, base_url: str) -> None:
    session_a = server.session(javascript_enabled=True)
    session_b = server.session(javascript_enabled=True)
    try:
        open_counter(session_a, base_url)
        open_counter(session_b, base_url)
        assert_local_htmx(session_a, base_url)
        assert_csp_authorized_htmx_forms(session_a)

        root_id = session_a.attribute(session_a.find("section.counter"), "id")
        initial_url = session_a.current_url()
        session_a.performance_logs()
        session_a.click_button("+1")
        assert_counter(session_a, "1")
        assert_csp_authorized_htmx_forms(session_a)

        assert session_a.current_url() == initial_url, "HTMX action unexpectedly navigated the page"
        assert session_a.attribute(session_a.find("section.counter"), "id") == root_id
        assert_hx_request(session_a.performance_logs())

        assert_counter(session_b, "0")
        session_b.click_button("-1")
        assert_counter(session_b, "-1")
        assert_counter(session_a, "1")

        session_a.click_button("Reset")
        assert_counter(session_a, "0")
        assert_counter(session_b, "-1")
    finally:
        session_a.close()
        session_b.close()


def exercise_js_off(server: DriverServer, base_url: str) -> None:
    session = server.session(javascript_enabled=False)
    try:
        open_counter(session, base_url)
        assert_local_htmx(session, base_url)

        session.performance_logs()
        session.click_button("+1")
        assert_counter(session, "1")
        assert session.current_url().rstrip("/") == f"{base_url}/counter"
        messages = session.performance_logs()
        assert_no_hx_request(messages)
        assert_prg_redirect(messages)

        session.refresh()
        assert_counter(session, "1")

        session.click_button("-1")
        assert_counter(session, "0")
        session.click_button("+1")
        assert_counter(session, "1")
        session.click_button("+1")
        assert_counter(session, "2")
        session.click_button("Reset")
        assert_counter(session, "0")
    finally:
        session.close()


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--base-url",
        default=os.environ.get("CLOG_COUNTER_URL", "http://127.0.0.1:8080"),
    )
    parser.add_argument("--driver", default=os.environ.get("CHROMEDRIVER_PATH"))
    parser.add_argument(
        "--browser",
        default=os.environ.get("CHROME_BINARY") or os.environ.get("CHROMIUM_PATH"),
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    base_url = args.base_url.rstrip("/")
    driver_path = find_executable(
        args.driver or os.environ.get("CHROMEWEBDRIVER"),
        ["chromedriver"],
        [
            "/usr/local/share/chromedriver-linux64/chromedriver",
            "/usr/local/bin/chromedriver",
            "/usr/bin/chromedriver",
        ],
    )
    browser_path = find_executable(
        args.browser,
        ["google-chrome", "google-chrome-stable", "chromium", "chromium-browser"],
        ["/usr/bin/google-chrome", "/usr/bin/chromium"],
    )

    with DriverServer(driver_path, browser_path) as server:
        exercise_js_on(server, base_url)
        exercise_js_off(server, base_url)

    print("HM-027 Counter real-browser acceptance passed")


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"HM-027 Counter browser acceptance FAILED: {exc}", file=sys.stderr)
        raise
