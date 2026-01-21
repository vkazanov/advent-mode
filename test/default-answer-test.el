;;; default-answer-test.el --- Answer at point reading  -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest default-answer-empty-buffer ()
  (with-temp-buffer
    (goto-char (point-min))
    (should (stringp (advent--default-answer)))
    (should (equal (advent--default-answer) ""))))

(ert-deftest default-answer-region-takes-precedence ()
  (with-temp-buffer
    (insert "xxx  42  yyy")
    ;; Select "  42  "
    (goto-char (+ (point-min) 3))
    (set-mark (point))
    (goto-char (+ (point-min) 9))
    (activate-mark)
    (let ((ans (advent--default-answer)))
      (should (stringp ans))
      (should (equal ans "42")))))

(ert-deftest default-answer-number-at-point ()
  (with-temp-buffer
    (insert "  123  ")
    ;; Put point inside the number.
    (goto-char (+ (point-min) 3))
    (let ((ans (advent--default-answer)))
      (should (stringp ans))
      (should (equal ans "123")))))

(ert-deftest default-answer-symbol-at-point ()
  (with-temp-buffer
    (insert "  foo  ")
    (goto-char (+ (point-min) 3))
    (let ((ans (advent--default-answer)))
      (should (stringp ans))
      (should (equal ans "foo")))))

(ert-deftest default-answer-line-fallback-trims-newline-and-spaces ()
  (with-temp-buffer
    ;; Ensure we don't hit number/symbol branches: point on whitespace.
    (insert "   abc def   \n")
    (goto-char (point-min)) ; on leading spaces
    (let ((ans (advent--default-answer)))
      (should (stringp ans))
      (should (equal ans "abc def")))))


(provide 'default-answer-test)
;;; default-answer-test.el ends here
