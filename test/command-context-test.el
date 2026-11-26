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

(ert-deftest advent-commands-reject-locked-puzzle-before-side-effects ()
  (ert-with-temp-directory advent-root-dir
    (let ((time (encode-time 59 59 4 1 12 2026 t))
          (advent-timezone "Asia/Tokyo")
          (unexpected (lambda (&rest _) (ert-fail "Acted on a locked puzzle"))))
      (cl-letf (((symbol-function 'current-time) (lambda () time))
                ((symbol-function 'advent--ensure-cookie-or-error) unexpected)
                ((symbol-function 'url-retrieve-synchronously) unexpected)
                ((symbol-function 'eww-browse-url) unexpected)
                ((symbol-function 'dired) unexpected))
        (dolist (call '((advent-fetch-input 2026 1) (advent-submit-answer "42" 1 2026 1)
                        (advent-browse-problem-page 2026 1) (advent-open-day 2026 1)
                        (advent-create-day 2026 1) (advent-today 2026 1) (advent-today)))
          (ert-info ((format "Calling %S" call))
            (let ((err (should-error (apply (car call) (cdr call)) :type 'user-error)))
              (should (string-match-p "2026-12-01.*05:00 UTC" (error-message-string err))))))
        (let ((default-directory (expand-file-name "year2026/day01/" advent-root-dir))
              (buffer-file-name nil))
          (should-not (file-exists-p default-directory))
          (make-directory default-directory t)
          (should-not (advent--context-year-day))
          (let ((err (should-error (advent-fetch-input) :type 'user-error)))
            (should (string-match-p "2026-12-01.*05:00 UTC" (error-message-string err)))))))))

(ert-deftest advent-prompt-caps-day-default-for-selected-year ()
  ;; In Los Angeles the local default is still the final day of 2024.
  (let ((time (encode-time 0 0 5 1 12 2025 t))
        (advent-timezone "America/Los_Angeles")
        defaults)
    (cl-letf (((symbol-function 'current-time) (lambda () time))
              ((symbol-function 'read-number)
               (lambda (prompt default)
                 (push default defaults)
                 (if (string= prompt "Year: ") 2025 default))))
      (should (equal (advent--prompt-year-day time) '(2025 1)))
      (should (equal (nreverse defaults) '(2024 1))))))

(ert-deftest advent-prompt-offers-no-day-default-for-unreleased-year ()
  (let ((time (encode-time 0 0 12 26 11 2026 t)))
    (cl-letf (((symbol-function 'current-time) (lambda () time))
              ((symbol-function 'read-number)
               (lambda (prompt default)
                 (if (string= prompt "Year: ") 2026
                   (should-not default)
                   1))))
      (let ((err (should-error (advent--prompt-year-day time) :type 'user-error)))
        (should (string-match-p "2026-12-01.*05:00 UTC" (error-message-string err)))))))

(ert-deftest advent-open-day-visits-selected-pair ()
  "Open the chosen day using the configured layout and completion labels."
  (ert-with-temp-directory advent-root-dir
    (let ((advent-year-dir-format "y%d")
          (advent-day-dir-format "d%02d")
          (advent-open-day-format "%04d/%02d")
          (time (encode-time 0 0 12 5 12 2024 t))
          (expected '("2023/05" "2024/05"))
          opened)
      (dolist (dir '("y2023/d05" "y2024/d05" "y2024/d06"))
        (make-directory (expand-file-name dir advent-root-dir) t))
      (cl-letf (((symbol-function 'current-time) (lambda () time))
                ((symbol-function 'completing-read)
                 (lambda (_prompt choices _predicate require-match _initial _history default)
                   (should require-match)
                   (should (equal (all-completions "" choices) expected))
                   (should (equal default (car (last expected))))
                   "2023/05"))
                ((symbol-function 'dired) (lambda (dir) (setq opened dir))))
        (ert-with-message-capture messages
          (advent-open-day)
          (should (string-empty-p messages)))
        (should (equal (directory-file-name opened)
                       (expand-file-name "y2023/d05" advent-root-dir)))
        (setq time (encode-time 0 0 5 6 12 2024 t)
              expected '("2023/05" "2024/05" "2024/06"))
        (advent-open-day)))))

(ert-deftest advent-open-day-missing-directory-names-puzzle-and-path ()
  (ert-with-temp-directory advent-root-dir
    (let ((advent-open-day-format "%d/%02d"))
      (should (equal (error-message-string
                      (should-error (advent-open-day 2024 5) :type 'user-error))
                     (format (concat "2024/05 - Directory does not exist at %s. "
                                     "Use M-x advent-create-day to create it")
                             (expand-file-name "year2024/day05" advent-root-dir)))))))

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
   (let ((years-and-days '(2024 5)) prompts opened)
     (cl-letf (((symbol-function 'read-number)
                (lambda (prompt &rest _)
                  (push prompt prompts)
                  (pop years-and-days)))
               ((symbol-function 'eww-browse-url)
                (lambda (url &rest _) (setq opened url))))
       (ert-with-message-capture messages
         (advent-browse-problem-page)
         (should (string-empty-p messages)))
       (should (equal (nreverse prompts) '("Year: " "Day for 2024: ")))
       (should (equal opened "https://adventofcode.com/2024/day/5"))))))

(ert-deftest advent-browse-problem-page-prefers-prefix-or-explicit-pair ()
  (let (opened)
    (cl-letf (((symbol-function 'advent--context-year-day) (lambda (&rest _) '(2023 1)))
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
              (lambda (_url file &rest _) (setq ,dst-var file) file))
             ((symbol-function 'find-file-other-window) #'ignore))
     ,@body))

(ert-deftest advent-fetch-input-without-root-errors-before-network ()
  "Fetching without a configured root names the missing setting."
  (let ((advent-root-dir nil)
        written)
    (advent-test--recording-fetch written
      (dolist (args '(nil (2024 5)))
        (let ((err (should-error (apply #'advent-fetch-input args) :type 'user-error)))
          (should (string-suffix-p "Set advent-root-dir to your Advent of Code solutions directory"
                                   (error-message-string err)))))
      (should-not written))))

(ert-deftest advent-fetch-input-explicit-day-writes-under-root ()
  "With explicit year/day, the input is saved under `advent-root-dir'."
  (advent-test--outside-day
   (let ((advent-input-file-name "puzzle.txt") written)
     (advent-test--recording-fetch written
       (cl-letf (((symbol-function 'advent--context-year-day)
                  (lambda () (ert-fail "Looked up context for an explicit pair"))))
         (ert-with-message-capture messages
           (advent-fetch-input 2024 5)
           (should (equal messages "Y2024/D05 - Downloaded puzzle.txt\n")))))
     (should (equal written
                    (expand-file-name "year2024/day05/puzzle.txt" root))))))

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
       (ert-with-message-capture messages
         (advent-fetch-input 2024 5)
         (should (string-empty-p messages))))
     (should (equal opened input)))))

(defun advent-test--input-response (&rest _)
  "Return a successful response as if retrieved by url.el."
  (let ((buffer (generate-new-buffer " *input*")))
    (with-current-buffer buffer (insert "HTTP/1.1 200 OK\n\ninput"))
    buffer))

(ert-deftest advent-create-day-fetches-input-under-root ()
  "Creating a day and accepting the download saves the input under root."
  (ert-with-temp-directory root
    (let ((advent-root-dir root)
          (advent-new-files '("absent.el"))
          (day-buffer (current-buffer))
          prompts warning opened)
      (with-temp-buffer
        (setq-local advent-open-day-format "AoC %d/%02d")
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt)
                     (push prompt prompts)
                     (string-match-p "Copy\\|Download" prompt)))
                  ((symbol-function 'warn)
                   (lambda (fmt &rest args) (setq warning (apply #'format fmt args))))
                  ((symbol-function 'dired) (lambda (_) (set-buffer day-buffer)))
                  ((symbol-function 'find-file-other-window) (lambda (file) (setq opened file)))
                  ((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
                  ((symbol-function 'url-retrieve-synchronously) #'advent-test--input-response))
          (ert-with-message-capture messages
            (save-current-buffer (advent-create-day 2024 5))
            (should (equal messages (concat "AoC 2024/05 - Created solution directory\n"
                                            "AoC 2024/05 - Downloaded input.txt\n"))))
          (ert-with-message-capture messages
            (advent-create-day 2024 5)
            (should (string-empty-p messages)))))
      (should (equal (nreverse prompts)
                     '("AoC 2024/05 - Copy template files here? "
                       "AoC 2024/05 - Open the problem page in EWW? "
                       "AoC 2024/05 - Download and open the input file? "
                       "AoC 2024/05 - Directory already exists. Open it? ")))
      (should (equal warning (format "AoC 2024/05 - Template file not found: %s"
                                     (expand-file-name "absent.el" root))))
      (should (file-directory-p (expand-file-name "year2024/day05/" root)))
      (should (equal opened (expand-file-name "year2024/day05/input.txt" root)))
      (should (equal (with-temp-buffer (insert-file-contents opened) (buffer-string))
                     "input")))))

(ert-deftest advent-fetch-input-failed-write-has-no-confirmation ()
  (ert-with-temp-directory advent-root-dir
    (cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
              ((symbol-function 'url-retrieve-synchronously) #'advent-test--input-response)
              ((symbol-function 'write-region)
               (lambda (&rest _) (signal 'file-error '("Disk full")))))
      (ert-with-message-capture messages
        (should (equal (should-error (advent-fetch-input 2024 5) :type 'file-error)
                       '(file-error "Disk full")))
        (should (string-empty-p messages))))))

(ert-deftest advent-fetch-input-missing-cookie-names-puzzle-and-login ()
  (ert-with-temp-directory advent-root-dir
    (let ((url-setup-done t) (advent-open-day-format "%d#%02d") prompt)
      (cl-letf (((symbol-function 'url-cookie-retrieve) #'ignore)
                ((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) nil)))
        (let ((err (should-error (advent-fetch-input 2024 5) :type 'user-error)))
          (should (string-prefix-p "2024#05 - AoC session cookie missing." prompt))
          (should (equal (error-message-string err)
                         "2024#05 - No AoC session cookie set; run M-x advent-login")))))))

(provide 'command-context-test)
;;; command-context-test.el ends here
