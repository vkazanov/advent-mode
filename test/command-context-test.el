;;; command-context-test.el --- Global vs local command behaviour  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ert-x)
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

(ert-deftest advent-commands-reject-partial-year-day ()
  "A partial pair fails before a command looks up or acts on a puzzle."
  (ert-with-temp-directory advent-root-dir
    (make-directory (expand-file-name "year2023/day05" advent-root-dir) t)
    (let ((unexpected (lambda (&rest _) (ert-fail "Used a fallback or opened a puzzle"))))
      (cl-letf (((symbol-function 'advent--context-year-day) unexpected)
                ((symbol-function 'advent--existing-days) unexpected)
                ((symbol-function 'advent--prompt-year-day) unexpected)
                ((symbol-function 'advent--puzzle-on-date) unexpected)
                ((symbol-function 'advent--ensure-cookie-or-error) unexpected)
                ((symbol-function 'dired) unexpected))
        (dolist (call '((advent-open-day) (advent-create-day) (advent-today)
                        (advent-browse-problem-page) (advent-fetch-input)
                        (advent-submit-answer "42" 1)))
          (dolist (pair '((2024 nil) (nil 5)))
            (ert-info ((format "Calling %S with %S" call pair))
              (let ((err (should-error (apply (car call) (append (cdr call) pair))
                                       :type 'user-error)))
                (should (equal (error-message-string err)
                               "Supply both YEAR and DAY"))))))))))

(ert-deftest advent-open-day-visits-selected-pair ()
  "Open the chosen day using the configured layout and completion labels."
  (ert-with-temp-directory advent-root-dir
    (let ((advent-year-dir-format "y%d")
          (advent-day-dir-format "d%02d")
          (advent-open-day-format "%04d/%02d")
          (time (encode-time 0 0 12 5 12 2024 t))
          opened)
      (dolist (dir '("y2023/d05" "y2024/d05"))
        (make-directory (expand-file-name dir advent-root-dir) t))
      (cl-letf (((symbol-function 'current-time) (lambda () time))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices _predicate require-match _initial _history default)
                   (should require-match)
                   (should (equal (all-completions "" choices) '("2023/05" "2024/05")))
                   (should (equal default "2024/05"))
                   "2023/05"))
                ((symbol-function 'dired) (lambda (dir) (setq opened dir))))
        (advent-open-day)
        (should (equal (directory-file-name opened)
                       (expand-file-name "y2023/d05" advent-root-dir)))))))

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

(ert-deftest advent-browse-problem-page-prefers-prefix-or-explicit-pair ()
  (let (opened)
    (cl-letf (((symbol-function 'advent--context-year-day) (lambda () '(2023 1)))
              ((symbol-function 'advent--prompt-year-day) (lambda (_) '(2024 5)))
              ((symbol-function 'eww-browse-url) (lambda (url) (setq opened url))))
      (advent-browse-problem-page)
      (should (equal opened (advent--problem-url 2023 1)))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'advent-browse-problem-page))
      (should (equal opened (advent--problem-url 2024 5)))
      (advent-browse-problem-page 2022 2)
      (should (equal opened (advent--problem-url 2022 2))))))

(defmacro advent-test--recording-fetch (dst-var &rest body)
  "Run BODY with network and UI stubbed; record the input path in DST-VAR."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
             ((symbol-function 'advent--write-url-to-file)
              (lambda (_url file) (setq ,dst-var file) file))
             ((symbol-function 'find-file-other-window) #'ignore))
     ,@body))

(ert-deftest advent-fetch-input-without-root-errors-before-network ()
  "Fetching without a configured root names the missing setting."
  (let ((advent-root-dir nil)
        written)
    (advent-test--recording-fetch written
      (dolist (args '(nil (2024 5)))
        (let ((err (should-error (apply #'advent-fetch-input args) :type 'user-error)))
          (should (string-match-p "advent-root-dir" (error-message-string err)))))
      (should-not written))))

(ert-deftest advent-fetch-input-explicit-day-writes-under-root ()
  "With explicit year/day, the input is saved under `advent-root-dir'."
  (advent-test--outside-day
   (let (written)
     (advent-test--recording-fetch written
       (cl-letf (((symbol-function 'advent--context-year-day)
                  (lambda () (ert-fail "Looked up context for an explicit pair"))))
         (advent-fetch-input 2024 5)))
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
