;;; login-test.el --- Tests for session cookie input  -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(defmacro advent-test--isolated-cookies (&rest body)
  "Run BODY with cookie storage local to it and url.el setup skipped.
No cookie file is read or written."
  (declare (indent 0))
  `(let ((url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (url-setup-done t))
     ,@body))

(ert-deftest advent-login-empty-prompt-signals-error ()
  "Pressing RET at the prefix-arg prompt must not store an empty cookie."
  (advent-test--isolated-cookies
   (let ((current-prefix-arg '(4)))
     (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "")))
       (should-error (call-interactively #'advent-login) :type 'user-error))
     (should-not (url-cookie-retrieve ".adventofcode.com" "/" t)))))

(ert-deftest advent-login-prompted-session-is-stored ()
  "A session entered at the prefix-arg prompt is stored."
  (advent-test--isolated-cookies
   (let ((current-prefix-arg '(4)))
     (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "abc123")))
       (call-interactively #'advent-login))
     (should (advent--cookie-ok-p)))))

(ert-deftest advent-session-prompt-trims-whitespace ()
  "Whitespace around a pasted session cookie is removed."
  (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "  abc123 \n")))
    (should (equal (advent-session-prompt) "abc123"))))

(ert-deftest advent-cookie-get-treats-empty-provider-as-missing ()
  "An empty string from the session provider counts as no cookie."
  (let ((advent-session-provider (lambda () "")))
    (should-not (advent--cookie-get))))

(provide 'login-test)
;;; login-test.el ends here
