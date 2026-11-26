;;; today-test.el --- Tests for opening today's puzzle  -*- lexical-binding: t; -*-

(require 'advent-mode)
(require 'cl-lib)
(require 'dired)
(require 'ert-x)

(ert-deftest advent-today-outside-event-explains-prefix-before-root-check ()
  (let ((advent-root-dir nil)
        (advent-timezone "America/Los_Angeles")
        (system-time-locale "C")
        ;; December 1 in New York, still November 30 in Los Angeles.
        (time (encode-time 0 59 7 1 12 2025 t)))
    (cl-letf (((symbol-function 'current-time)
               (lambda () (prog1 time (setq time (time-add time 60))))))
      (should (equal
               (concat "No Advent of Code puzzle opens on November 30. "
                       "Use C-u M-x advent-today to choose a year and day")
               (error-message-string
                (should-error (advent-today) :type 'user-error)))))))

(ert-deftest advent-today-explicit-arguments-need-a-pair-and-root ()
  (dolist (args '((2024) (nil 1) (2024 26)))
    (let ((advent-root-dir temporary-file-directory))
      (should-error (apply #'advent-today args) :type 'user-error)))
  (let ((advent-root-dir nil))
    (should-error (advent-today 2024 1) :type 'user-error)))

(ert-deftest advent-today-creates-once-and-revisits-in-dired ()
  (ert-with-temp-directory root
    (let* ((advent-root-dir root)
           (advent-new-files '("solution.el"))
           (advent-timezone "America/New_York")
           (time (encode-time 0 0 5 1 12 2025 t))
           (dir (expand-file-name "year2025/day01/" root))
           (solution (expand-file-name "solution.el" dir))
           (template (expand-file-name "solution.el" root))
           (page (generate-new-buffer " *advent test page*"))
           urls)
      (with-temp-file template (insert "template"))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (cl-letf (((symbol-function 'current-time) (lambda () time))
                      ((symbol-function 'eww-browse-url)
                       (lambda (url)
                         (push url urls)
                         (pop-to-buffer-same-window page)))
                      ((symbol-function 'advent-fetch-input)
                       (lambda (&rest _) (ert-fail "Unexpected input request")))
                      ((symbol-function 'advent--ensure-cookie-or-error)
                       (lambda () (ert-fail "Unexpected authentication"))))
              (ert-with-message-capture messages
                (call-interactively #'advent-today)
                (should (string-match-p "Y2025/D01 — Created solution directory\n"
                                        messages)))
              (should (eq page (window-buffer (selected-window))))
              (should (get-buffer-window (dired-find-buffer-nocreate dir)))
              (should (equal (with-temp-buffer (insert-file-contents solution)
                                               (buffer-string))
                             "template"))
              (with-temp-file solution (insert "edited"))
              (let ((windows (window-list))
                    (other-buffer (window-buffer (next-window))))
                (advent-today)
                (should (equal windows (window-list)))
                (should (eq other-buffer (window-buffer (next-window))))
                (should (eq (current-buffer) (dired-find-buffer-nocreate dir))))
              (should (equal urls '("https://adventofcode.com/2025/day/1")))
              (should (equal (with-temp-buffer (insert-file-contents solution)
                                               (buffer-string))
                             "edited"))))
        (kill-buffer page)
        (when-let* ((buffer (dired-find-buffer-nocreate dir)))
          (kill-buffer buffer))))))

(ert-deftest advent-today-prefix-opens-precreated-empty-day-off-season ()
  (ert-with-temp-directory root
    (let ((advent-root-dir root)
          (dir (expand-file-name "year2024/day05" root))
          (time (encode-time 0 0 12 1 7 2025 t))
          (current-prefix-arg '(4))
          opened)
      (make-directory dir t)
      (cl-letf (((symbol-function 'current-time) (lambda () time))
                ((symbol-function 'advent--prompt-year-day)
                 (lambda (now) (should (equal now time)) '(2024 5)))
                ((symbol-function 'dired) (lambda (path) (setq opened path)))
                ((symbol-function 'eww-browse-url)
                 (lambda (&rest _) (ert-fail "Unexpected browser visit"))))
        (ert-with-message-capture messages
          (call-interactively #'advent-today)
          (should (string-empty-p messages)))
        (should (equal dir opened))))))

(ert-deftest advent-today-keeps-directory-visible-on-browser-error ()
  (ert-with-temp-directory root
    (let ((advent-root-dir root)
          (advent-new-files nil)
          (dir (expand-file-name "year2024/day05/" root)))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (cl-letf (((symbol-function 'eww-browse-url)
                       (lambda (_url)
                         (switch-to-buffer (get-buffer "*scratch*"))
                         (error "Browser failed"))))
              (should-error (advent-today 2024 5))
              (let ((buffer (dired-find-buffer-nocreate dir)))
                (should buffer)
                (should (get-buffer-window buffer)))))
        (when-let* ((buffer (dired-find-buffer-nocreate dir)))
          (kill-buffer buffer))))))

(provide 'today-test)
;;; today-test.el ends here
