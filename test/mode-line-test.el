;;; mode-line-test.el --- Tests for Advent mode-line formatting
(require 'advent-mode)
(require 'ert)
(require 'cl-lib)

(ert-deftest mode-line-string-test ()
  "Test that mode-line string matches format when cookie is OK."
  (cl-letf (((symbol-function 'advent--cookie-status-string)
             (lambda () "✓")))
    (should (string=
             (advent--mode-line 2024 5)
             (format advent-mode-line-format 2024 "05" "✓")))))

(ert-deftest mode-line-string-test-fail-cookie ()
  "Test mode-line string when cookie check fails."
  (cl-letf (((symbol-function 'advent--cookie-status-string)
             (lambda () "✗")))
    (should (string=
             (advent--mode-line 2024 5)
             (format advent-mode-line-format 2024 "05" "✗")))))
;;; mode-line-test.el ends here
