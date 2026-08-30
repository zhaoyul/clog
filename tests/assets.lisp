;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime vendored-asset tests                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defparameter +hm-003-assets+
  '(("htmx.min.js" . "e484d9171a9db30a39c8f16e3d709d4137f3211c659f8e6125816635033d593f")
    ("hx-sse.min.js" . "8a834680c4000a9034d79228872372a92e140c810a075cb6d4a76690dfc13085")
    ("hx-ws.min.js" . "a7c11e4eca05417d6299bb40aaacca01572e44605389fc4d5ef12be408a4d03b")
    ("hx-csp.min.js" . "279f659b9ec8658e3d47230cc545b0bb4e3efa4c2d780454d54db104a5257b7d")
    ("LICENSE" . "d3d2456f76414f2456104660ebd65aff1c04cd7966b942bdabd63f3cdb316a38")))

(defparameter +hm-003-javascript-files+
  '("htmx.min.js" "hx-sse.min.js" "hx-ws.min.js" "hx-csp.min.js"))

(defun hm-003-repository-root ()
  (asdf:system-source-directory :clog))

(defun hm-003-vendor-root ()
  (merge-pathnames "static-files/vendor/htmx/4.0.0/"
                   (hm-003-repository-root)))

(defun hm-003-vendor-path (file)
  (merge-pathnames file (hm-003-vendor-root)))

(defun hm-003-sha256 (path)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-file :sha256 path))))

(defun hm-003-read-file (path)
  (uiop:read-file-string path))

(test assets/files/versioned-local-layout
  (is (search "/static-files/vendor/htmx/4.0.0/"
              (uiop:unix-namestring (hm-003-vendor-root))))
  (dolist (entry +hm-003-assets+)
    (is (probe-file (hm-003-vendor-path (car entry)))
        "Vendored file ~A must exist." (car entry)))
  (is (probe-file (hm-003-vendor-path "SHA256SUMS"))))

(test assets/checksum/pinned-digests-match-vendored-bytes
  (dolist (entry +hm-003-assets+)
    (is (string= (cdr entry)
                 (hm-003-sha256 (hm-003-vendor-path (car entry))))
        "Vendored asset ~A differs from the pinned SHA-256."
        (car entry))))

(test assets/checksum/manifest-and-provenance-match-pins
  (let ((manifest (hm-003-read-file (hm-003-vendor-path "SHA256SUMS")))
        (document (hm-003-read-file
                   (merge-pathnames "docs/dependencies/htmx.md"
                                    (hm-003-repository-root)))))
    (is (search "htmx-4.0.0-dist.zip" document))
    (is (search "858d5fb806ed3003704bc9c9a7fc7aad15213dcfc78c5b78b005fff4211ce57c"
                document))
    (dolist (entry +hm-003-assets+)
      (is (search (car entry) manifest))
      (is (search (cdr entry) manifest))
      (is (search (car entry) document))
      (is (search (cdr entry) document)))))

(test assets/mime/javascript-assets-map-to-javascript
  (dolist (file +hm-003-javascript-files+)
    (let ((mime (mimes:mime-lookup (hm-003-vendor-path file))))
      (is (member mime '("application/javascript" "text/javascript")
                  :test #'string=)
          "~A must map to a JavaScript MIME type, got ~S."
          file mime))))

(test assets/runtime/no-cdn-reference-and-core-is-4.0.0
  (dolist (file +hm-003-javascript-files+)
    (let ((content (string-downcase
                    (hm-003-read-file (hm-003-vendor-path file)))))
      (is-false (search "unpkg.com" content))
      (is-false (search "cdn.jsdelivr.net" content))
      (is-false (search "cdnjs.cloudflare.com" content))))
  (is (search "4.0.0"
              (hm-003-read-file (hm-003-vendor-path "htmx.min.js")))))
