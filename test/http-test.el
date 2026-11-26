;;; http-test.el --- HTTP helper tests               -*- lexical-binding: t; -*-

(require 'ert)
(require 'ert-x)
(require 'advent-mode)
(require 'cl-lib)

(defun advent-test--make-http-buffer (text &optional status)
  (let ((b (generate-new-buffer " *advent-http-test*")))
    (with-current-buffer b
      (insert text)
      (when status
        (setq-local url-http-response-status status)))
    b))

(ert-deftest http-request-200-returns-body ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "HTTP/1.1 200 OK\r\nX: y\r\n\r\nhello\n" 200))))
    (should (equal (advent--http-request "http://x/" "2024/05" "GET") "hello\n"))))

(ert-deftest http-request-uses-status-line-if-var-missing ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               ;; no url-http-response-status set
               (advent-test--make-http-buffer
                "HTTP/1.1 200 OK\r\n\r\nok"))))
    (should (equal (advent--http-request "http://x/" "2024/05" "GET") "ok"))))

(ert-deftest http-request-errors-on-4xx-5xx ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "HTTP/1.1 404 Not Found\r\n\r\nnope" 404))))
    (let ((advent-open-day-format "[%d.%02d]"))
      (cl-letf (((symbol-function 'advent--cookie-ok-p) #'always))
        (should (equal (error-message-string
                        (should-error (advent-submit-answer "42" 2 2024 5)))
                       "[2024.05] — HTTP 404: nope"))))))

(ert-deftest http-request-errors-on-malformed-no-separator ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "HTTP/1.1 200 OK\r\nX: y\r\n<body-no-separator>" 200))))
    (should (equal (error-message-string
                    (should-error (advent--http-request "http://x/" "2024/05" "GET")))
                   "2024/05 — Malformed HTTP response (no header/body separator)"))))

(ert-deftest http-request-errors-on-missing-status ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "GARBAGE\r\n\r\nhi"))))
    (should (equal (error-message-string
                    (should-error (advent--http-request "http://x/" "2024/05" "GET")))
                   "2024/05 — Malformed HTTP response (no status)"))))

(ert-deftest http-fetch-without-response-names-target ()
  (ert-with-temp-directory advent-root-dir
    (cl-letf (((symbol-function 'url-retrieve-synchronously) #'ignore)
              ((symbol-function 'advent--cookie-ok-p) #'always))
      (should (equal (error-message-string
                      (should-error (advent-fetch-input 2024 5)))
                     "Y2024/D05 — Failed to GET https://adventofcode.com/2024/day/5/input")))))

(ert-deftest write-url-to-file-requires-nonempty-body ()
  (let ((tmp (make-temp-file "advent-" t))
        (dst nil))
    (setq dst (expand-file-name "input.txt" tmp))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (advent-test--make-http-buffer
                  "HTTP/1.1 200 OK\r\n\r\n   \n" 200))))
      (should (equal (error-message-string
                      (should-error (advent--write-url-to-file "http://x/" dst "2024/05")))
                     "2024/05 — Empty HTTP response body"))
      (should-not (file-exists-p dst)))))

(ert-deftest write-url-to-file-writes-body ()
  (let ((tmp (make-temp-file "advent-" t))
        (dst nil))
    (setq dst (expand-file-name "input.txt" tmp))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (advent-test--make-http-buffer
                  "HTTP/1.1 200 OK\r\n\r\nabc\n" 200))))
      (should (equal (advent--write-url-to-file "http://x/" dst "2024/05") dst))
      (should (equal (with-temp-buffer
                       (insert-file-contents dst)
                       (buffer-string))
                     "abc\n")))))


(provide 'http-test)
;;; http-test.el ends here
