;;; submit-answer-test.el --- Guards against empty or padded answers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(defmacro advent-test--in-day (year day &rest body)
  "Run BODY with `default-directory' inside a temporary YEAR/DAY directory."
  (declare (indent 2))
  `(let* ((root (make-temp-file "aoc-root-" t))
          (advent-root-dir root)
          (daydir (expand-file-name (format "year%d/day%02d/" ,year ,day) root))
          (default-directory daydir)
          (buffer-file-name nil))
     (make-directory daydir t)
     ,@body))

(ert-deftest advent-submit-empty-answer-is-rejected ()
  "An empty answer signals `user-error' before any request is made."
  (advent-test--in-day 2024 5
    (cl-letf (((symbol-function 'advent--ensure-cookie-or-error)
               (lambda () (error "cookie checked")))
              ((symbol-function 'advent--http-post)
               (lambda (&rest _) (error "posted")))
              ((symbol-function 'display-buffer) #'ignore))
      (should-error (advent-submit-answer "   " "1" 2024 5) :type 'user-error))))

(ert-deftest advent-submit-trims-answer ()
  "Surrounding whitespace is removed before the answer is posted."
  (advent-test--in-day 2024 5
    (let (posted)
      (cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
                ((symbol-function 'advent--http-post)
                 (lambda (_url data) (setq posted data) "ok"))
                ((symbol-function 'display-buffer) #'ignore))
        (advent-submit-answer "  42 " "1" 2024 5))
      (should (equal posted "level=1&answer=42")))))

(ert-deftest advent-submit-interactive-empty-answer-stops-before-level ()
  "An empty answer typed at the prompt is rejected before the level is asked."
  (advent-test--in-day 2024 5
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (error "level prompted"))))
      (should-error (call-interactively #'advent-submit-answer) :type 'user-error))))

(provide 'submit-answer-test)
;;; submit-answer-test.el ends here
