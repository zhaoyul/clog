#!/usr/bin/env python3
"""HM-027 real-browser acceptance for the CLOG Hypermedia Counter.

Start the Lisp example first and provide CLOG_COUNTER_URL when it is not using
the default URL. The suite exercises JavaScript-on HTMX morphing, independent
browser sessions and the JavaScript-off Post/Redirect/Get fallback.
"""

import os
from urllib.parse import urlparse

from playwright.sync_api import expect, sync_playwright


BASE_URL = os.environ.get("CLOG_COUNTER_URL", "http://127.0.0.1:8080").rstrip("/")
CHROMIUM_PATH = os.environ.get("CHROMIUM_PATH", "/usr/bin/chromium")


def counter_value(page):
    return page.locator("#counter-value")


def open_counter(context):
    page = context.new_page()
    page.goto(f"{BASE_URL}/counter", wait_until="domcontentloaded")
    expect(counter_value(page)).to_have_text("0")
    return page


def assert_local_htmx(page):
    script = page.locator('script[src*="/vendor/htmx/4.0.0/htmx.min.js"]')
    expect(script).to_have_count(1)
    src = script.get_attribute("src")
    assert src is not None
    assert urlparse(src).scheme == "", f"HTMX must be same-origin, got {src!r}"


def exercise_js_on(browser):
    context_a = browser.new_context(java_script_enabled=True)
    context_b = browser.new_context(java_script_enabled=True)
    try:
        page_a = open_counter(context_a)
        page_b = open_counter(context_b)
        assert_local_htmx(page_a)

        action_requests = []

        def record_action(request):
            if "/_clog/action/" in request.url:
                action_requests.append(request)

        page_a.on("request", record_action)
        root_id = page_a.locator("section.counter").get_attribute("id")
        initial_url = page_a.url

        page_a.get_by_role("button", name="+1", exact=True).click()
        expect(counter_value(page_a)).to_have_text("1")

        assert page_a.url == initial_url, "HTMX action unexpectedly navigated the page"
        assert page_a.locator("section.counter").get_attribute("id") == root_id
        assert action_requests, "No HTMX action request was observed"
        assert any(
            request.headers.get("hx-request", "").lower() == "true"
            for request in action_requests
        ), "Action request did not carry HX-Request: true"

        # A second browser context owns an independent Lack session/component.
        expect(counter_value(page_b)).to_have_text("0")
        page_b.get_by_role("button", name="-1", exact=True).click()
        expect(counter_value(page_b)).to_have_text("-1")
        expect(counter_value(page_a)).to_have_text("1")

        page_a.get_by_role("button", name="Reset", exact=True).click()
        expect(counter_value(page_a)).to_have_text("0")
        expect(counter_value(page_b)).to_have_text("-1")
    finally:
        context_a.close()
        context_b.close()


def exercise_js_off(browser):
    context = browser.new_context(java_script_enabled=False)
    try:
        page = open_counter(context)
        assert_local_htmx(page)

        page.get_by_role("button", name="+1", exact=True).click()
        expect(counter_value(page)).to_have_text("1")
        assert page.url.rstrip("/") == f"{BASE_URL}/counter"

        # PRG means a browser refresh is a GET and cannot repeat the previous POST.
        page.reload(wait_until="domcontentloaded")
        expect(counter_value(page)).to_have_text("1")

        page.get_by_role("button", name="-1", exact=True).click()
        expect(counter_value(page)).to_have_text("0")
        page.get_by_role("button", name="+1", exact=True).click()
        page.get_by_role("button", name="+1", exact=True).click()
        expect(counter_value(page)).to_have_text("2")
        page.get_by_role("button", name="Reset", exact=True).click()
        expect(counter_value(page)).to_have_text("0")
    finally:
        context.close()


def main():
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(
            headless=True,
            executable_path=CHROMIUM_PATH,
            args=["--no-sandbox"],
        )
        try:
            exercise_js_on(browser)
            exercise_js_off(browser)
        finally:
            browser.close()

    print("HM-027 Counter browser acceptance passed")


if __name__ == "__main__":
    main()
