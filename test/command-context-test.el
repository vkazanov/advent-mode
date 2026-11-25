;;; command-context-test.el --- Global vs local command behaviour  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
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

(defmacro advent-test--recording-fetch (dst-var &rest body)
  "Run BODY with network and UI stubbed; record the input path in DST-VAR."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
             ((symbol-function 'advent--write-url-to-file)
              (lambda (_url file) (setq ,dst-var file) file))
             ((symbol-function 'find-file-other-window) #'ignore))
     ,@body))

(ert-deftest advent-fetch-input-without-root-errors-before-network ()
  "With explicit year/day but no root, fetching fails without any request."
  (let ((advent-root-dir nil)
        written)
    (advent-test--recording-fetch written
      (should-error (advent-fetch-input 2024 5) :type 'user-error)
      (should-not written))))

(ert-deftest advent-fetch-input-explicit-day-writes-under-root ()
  "With explicit year/day, the input is saved under `advent-root-dir'."
  (advent-test--outside-day
   (let (written)
     (advent-test--recording-fetch written
       (advent-fetch-input 2024 5))
     (should (equal written
                    (expand-file-name "year2024/day05/input.txt" root))))))

(ert-deftest advent-fetch-input-existing-input-opens-without-cookie-or-network ()
  "An input already on disk is opened without checking the cookie or downloading."
  (advent-test--outside-day
   (let ((input (expand-file-name "year2024/day05/input.txt" root))
         opened)
     (make-directory (file-name-directory input) t)
     (with-temp-file input (insert "1 2 3\n"))
     (cl-letf (((symbol-function 'advent--ensure-cookie-or-error)
                (lambda () (error "cookie must not be checked")))
               ((symbol-function 'advent--write-url-to-file)
                (lambda (&rest _) (error "network must not be called")))
               ((symbol-function 'find-file-other-window)
                (lambda (file &rest _) (setq opened file))))
       (advent-fetch-input 2024 5))
     (should (equal opened input)))))

(ert-deftest advent-create-day-fetches-input-under-root ()
  "Creating a day and accepting the download saves the input under root."
  (advent-test--outside-day
   (let ((advent-new-files nil)
         written)
     (advent-test--recording-fetch written
       (cl-letf (((symbol-function 'y-or-n-p)
                  (lambda (prompt) (string-match-p "Download" prompt)))
                 ((symbol-function 'dired) #'ignore))
         (advent-create-day 2024 5)))
     (should (file-directory-p (expand-file-name "year2024/day05/" root)))
     (should (equal written
                    (expand-file-name "year2024/day05/input.txt" root))))))

(provide 'command-context-test)
;;; command-context-test.el ends here
