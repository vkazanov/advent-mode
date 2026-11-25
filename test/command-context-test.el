;;; command-context-test.el --- Global vs local command behaviour  -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(defmacro advent-test--outside-day (&rest body)
  "Run BODY with `advent-root-dir' set and the context outside any day dir."
  (declare (indent 0))
  `(let* ((root (make-temp-file "aoc-root-" t))
          (advent-root-dir root)
          (elsewhere (make-temp-file "aoc-elsewhere-" t))
          (default-directory (file-name-as-directory elsewhere))
          (buffer-file-name nil))
     ,@body))

(ert-deftest advent-fetch-input-outside-day-errors-before-network ()
  "Outside a day directory, fetching the input fails without any request."
  (advent-test--outside-day
   (cl-letf (((symbol-function 'advent--write-url-to-file)
              (lambda (&rest _) (error "network must not be called"))))
     (should-error (advent-fetch-input) :type 'user-error))))

(ert-deftest advent-submit-answer-outside-day-errors-before-prompting ()
  "Outside a day directory, submitting fails before asking for an answer."
  (advent-test--outside-day
   (cl-letf (((symbol-function 'read-string)
              (lambda (&rest _) (error "must not prompt for the answer")))
             ((symbol-function 'completing-read)
              (lambda (&rest _) (error "must not prompt for the level"))))
     (should-error (call-interactively #'advent-submit-answer)
                   :type 'user-error))))

(ert-deftest advent-missing-context-error-names-the-fix ()
  "The missing-context error tells the user how to get out of the situation."
  (advent-test--outside-day
   (let ((msg (should-error (advent--ensure-context-or-error nil nil)
                            :type 'user-error)))
     (should (string-match-p "advent-open-day" (error-message-string msg))))))

(ert-deftest advent-context-from-day-dir-fills-missing-args ()
  "Inside a day directory, the context supplies year and day."
  (let* ((root (make-temp-file "aoc-root-" t))
         (advent-root-dir root)
         (daydir (expand-file-name "year2024/day05/" root))
         (default-directory daydir)
         (buffer-file-name nil))
    (make-directory daydir t)
    (should (equal (advent--ensure-context-or-error nil nil) '(2024 5)))))

(ert-deftest advent-browse-problem-page-outside-day-prompts ()
  "Browsing works outside a day directory by prompting for year and day."
  (advent-test--outside-day
   (let (opened)
     (cl-letf (((symbol-function 'advent--prompt-year-day)
                (lambda (_time) (list 2024 5)))
               ((symbol-function 'eww-browse-url)
                (lambda (url &rest _) (setq opened url))))
       (advent-browse-problem-page)
       (should (equal opened "https://adventofcode.com/2024/day/5"))))))

(provide 'command-context-test)
;;; command-context-test.el ends here
