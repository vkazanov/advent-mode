;;; login-test.el --- Tests for session cookie input  -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(defmacro advent-test--isolated-cookies (&rest body)
  "Run BODY with cookie storage local to it and url.el setup skipped.
No cookie file is read or written."
  (declare (indent 0))
  `(let ((url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (url-cookie-file nil)
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

(ert-deftest advent-login-from-submission-keeps-label-in-cookie-prompt ()
  "The built-in provider can identify the puzzle that requested login."
  (let ((advent-session-provider #'advent-session-prompt)
        (advent-open-day-format "%d/%02d")
        prompt)
    (cl-letf (((symbol-function 'url-cookie-retrieve) #'ignore)
              ((symbol-function 'y-or-n-p) #'always)
              ((symbol-function 'read-passwd) (lambda (p) (setq prompt p) "")))
      (should (equal (error-message-string
                      (should-error (advent-submit-answer "42" 2 2024 5) :type 'user-error))
                     "2024/05 — No session cookie entered"))
      (should (equal prompt "2024/05 — Advent of Code session cookie: ")))))

(ert-deftest advent-cookie-get-treats-empty-provider-as-missing ()
  "An empty string from the session provider counts as no cookie."
  (let ((advent-session-provider (lambda () "")))
    (should-not (advent--cookie-get))))

;;; Reset

(ert-deftest advent-login-reset-removes-session-cookie ()
  "After a reset no AoC session cookie is left."
  (advent-test--isolated-cookies
   (advent-login "abc123")
   (advent-login-reset)
   (should-not (advent--cookie-ok-p))
   (should-not (url-cookie-retrieve ".adventofcode.com" "/" t))))

(ert-deftest advent-login-and-reset-update-all-lighters-once ()
  (advent-test--isolated-cookies
   (with-temp-buffer
     (let ((first (current-buffer))
           (lookups 0)
           updates)
       (cl-letf (((symbol-function 'advent--context-year-day)
                  (lambda ()
                    (cl-incf lookups)
                    (if (eq (current-buffer) first) '(2024 5) '(2023 1)))))
         (advent-mode 1)
         (with-temp-buffer
           (advent-mode 1)
           (cl-flet ((check (glyph)
                       (should (equal (with-current-buffer first (advent--mode-line))
                                      (format " AoC[Y2024/D05 %s]" glyph)))
                       (should (equal (advent--mode-line)
                                      (format " AoC[Y2023/D01 %s]" glyph)))
                       (should (= lookups 2))))
             (cl-letf (((symbol-function 'force-mode-line-update)
                        (lambda (&optional all) (push all updates))))
               (check "✗")
               (advent-login "abc123")
               (should (equal updates '(t)))
               (check "✓")
               (setq updates nil)
               (advent-login-reset)
               (should (equal updates '(t)))
               (check "✗")))))))))

(ert-deftest advent-login-reset-keeps-other-domains ()
  "Cookies of other domains, even look-alike ones, survive a reset."
  (advent-test--isolated-cookies
   (advent-login "abc123")
   (dolist (domain '(".example.com" ".notadventofcode.com"))
     (url-cookie-store "id" "x" "Fri, 25 Dec 2031 00:00:00 GMT" domain "/" t))
   (advent-login-reset)
   (dolist (domain '(".example.com" ".notadventofcode.com"))
     (should (url-cookie-retrieve domain "/" t)))))

(provide 'login-test)
;;; login-test.el ends here
