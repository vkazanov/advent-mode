;;; http-test.el --- HTTP helper tests               -*- lexical-binding: t; -*-

(require 'ert)
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
    (should (equal (advent--http-request "http://x/" "GET") "hello\n"))))

(ert-deftest http-request-uses-status-line-if-var-missing ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               ;; no url-http-response-status set
               (advent-test--make-http-buffer
                "HTTP/1.1 200 OK\r\n\r\nok"))))
    (should (equal (advent--http-request "http://x/" "GET") "ok"))))

(ert-deftest http-request-errors-on-4xx-5xx ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "HTTP/1.1 404 Not Found\r\n\r\nnope" 404))))
    (should-error (advent--http-request "http://x/" "GET")
                  :type 'error)))

(ert-deftest http-request-errors-on-malformed-no-separator ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "HTTP/1.1 200 OK\r\nX: y\r\n<body-no-separator>" 200))))
    (should-error (advent--http-request "http://x/" "GET")
                  :type 'error)))

(ert-deftest http-request-errors-on-missing-status ()
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (&rest _)
               (advent-test--make-http-buffer
                "GARBAGE\r\n\r\nhi"))))
    (should-error (advent--http-request "http://x/" "GET")
                  :type 'error)))

(ert-deftest write-url-to-file-requires-nonempty-body ()
  (let ((tmp (make-temp-file "advent-" t))
        (dst nil))
    (setq dst (expand-file-name "input.txt" tmp))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (advent-test--make-http-buffer
                  "HTTP/1.1 200 OK\r\n\r\n   \n" 200))))
      (should-error (advent--write-url-to-file "http://x/" dst) :type 'error)
      (should-not (file-exists-p dst)))))

(ert-deftest write-url-to-file-writes-body ()
  (let ((tmp (make-temp-file "advent-" t))
        (dst nil))
    (setq dst (expand-file-name "input.txt" tmp))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (advent-test--make-http-buffer
                  "HTTP/1.1 200 OK\r\n\r\nabc\n" 200))))
      (should (equal (advent--write-url-to-file "http://x/" dst) dst))
      (should (equal (with-temp-buffer
                       (insert-file-contents dst)
                       (buffer-string))
                     "abc\n")))))


(provide 'http-test)
;;; http-test.el ends here
