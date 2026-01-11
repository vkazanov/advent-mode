;;; advent.el --- advent of code utils               -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Keegan Carruthers-Smith
;; 2024-2026  Vladimir Kazanov

;; Author: Vladimir Kazanov <vekazanov@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A set of little advent of code helpers, based on
;; https://github.com/keegancsmith/advent/
;;
;; Further ideas:
;; TODO: check problem creation and submission.
;; TODO: template/util should be per-year
;; TODO: templates should assume input-test.txt and input.txt
;; TODO: easy test suite
;; TODO: simple benchmark to run all code per year with undestandable output
;; TODO: document the helper, expected project structure and session setting

;;; Code:

(require 'url)

(defgroup advent nil
  "A customization group for the Advent of Code helper library."
  :group 'convenience)

(defcustom advent-dir
  (expand-file-name "~/projects-my/advent-of-code/")
  "Root directory containing Advent of Code solutions."
  :type 'directory
  :group 'advent)

(defcustom advent-file-template
  (file-name-concat advent-dir "template.py")
  "Template used to create a new AoC day’s solution file."
  :type 'file
  :group 'advent)

(defcustom advent-lib-template
  (file-name-concat advent-dir "util.py")
  "Template for shared AoC utility code."
  :type 'file
  :group 'advent)

(defcustom advent-mode-line-format " AoC[%s/%s %s]"
  "Format string used in the mode line.
It receives (YEAR DAY COOKIE-STATUS)."
  :type 'string
  :group 'advent)

(defvar advent-submit-level-history nil
  "History of level submission.")

;;;; Utilities

(defun advent--day ()
  "Return current AoC day (America/New_York)."
  (nth 3 (decode-time (current-time) "America/New_York")))

(defun advent--default-answer ()
  "Return current region contents as a default answer."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

;;;; Cookie management

(defun advent-login (session)
  "Store SESSION cookie for adventofcode.com."
  (interactive "sSession cookie (from browser): ")
  (url-cookie-store "session" session
                    "Thu, 25 Dec 2027 20:17:36 -0000"
                    ".adventofcode.com" "/" t)
  (advent--refresh-mode-lines)
  (message "AoC session cookie stored."))

(defun advent--cookie-ok-p ()
  "Return non-nil if an AoC session cookie exists and is not expired."
  (when-let ((cookies (url-cookie-retrieve ".adventofcode.com" "/" t)))
    (not (url-cookie-expired-p (car cookies)))))

(defun advent--cookie-status ()
  "Return a character cookie representing cookie session status."
  (if (advent--cookie-ok-p) "✓" "✗"))

;;;; Problem and input handling

(defun advent (prefix &optional year day)
  "Open AoC problem page and input.
With PREFIX, prompt for YEAR and DAY; otherwise infer from today."
  (interactive "P")
  (if prefix
      (setq year (read-string "Year: " (format-time-string "%Y"))
            day  (read-number "Day: " (advent--day)))
    (setq year (or year (format-time-string "%Y"))
          day  (or day (advent--day))))
  (eww (format "https://adventofcode.com/%s/day/%d" year day))
  (advent-src year day)
  (advent-input year day)
  (switch-to-buffer "*eww*"))

(defun advent-submit (answer level &optional year day)
  "Submit ANSWER for LEVEL (1 or 2) to adventofcode.com.
Optional YEAR and DAY default to today."
  (interactive
   (list
    (let ((default (advent--default-answer)))
      (read-string (if (and default (not (string-empty-p default)))
                       (format "Submit (default %s): " default)
                     "Submit: ")
                   nil nil default))
    (let ((default (or (car advent-submit-level-history) "1")))
      (read-string (format "Level (%s): " default)
                   nil 'advent-submit-level-history default))))
  (let* ((year (or year (format-time-string "%Y")))
         (day  (or day (advent--day)))
         (url  (format "https://adventofcode.com/%s/day/%d/answer" year day))
         (url-request-method "POST")
         (url-request-data (format "level=%s&answer=%s" level answer))
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (eww-browse-url url)))

(defun advent-src (&optional year day)
  "Open AoC source file for YEAR and DAY, creating from template if needed."
  (interactive "P")
  (let* ((year (or year (format-time-string "%Y")))
         (day  (number-to-string (or day (advent--day))))
         (dir  (file-name-concat advent-dir year day))
         (file1 (file-name-concat dir "part1.py"))
         (file2 (file-name-concat dir "part2.py")))
    (unless (file-exists-p file1)
      (mkdir dir t)
      (when (file-exists-p advent-file-template)
        (copy-file advent-file-template file1)
        (copy-file advent-file-template file2))
      (when (file-exists-p advent-lib-template)
        (copy-file advent-lib-template dir)))
    (find-file file1)))

(defun advent-input (&optional year day)
  "Fetch or open AoC input.txt for YEAR and DAY in another window."
  (interactive "P")
  (let* ((year (or year (format-time-string "%Y")))
         (day  (number-to-string (or day (advent--day))))
         (url  (format "https://adventofcode.com/%s/day/%s/input" year day))
         (dir  (file-name-concat advent-dir year day))
         (file (file-name-concat dir "input.txt")))
    (if (file-exists-p file)
        (find-file-other-window file)
      (url-retrieve url #'advent--download-callback (list file)))))

(defun advent--download-callback (status file)
  "Save downloaded input to FILE.  STATUS is the request result."
  (if (plist-get status :error)
      (message "Failed to download input: %S" (plist-get status :error))
    (mkdir (file-name-directory file) t)
    (goto-char (point-min))
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (write-region (point) (point-max) file))
    (find-file-other-window file)))

;;;; Mode-line helpers

(defun advent--in-aoc-project-p (&optional dir)
  "Return non-nil if DIR (default `default-directory') is under `advent-dir'."
  (file-in-directory-p (expand-file-name (or dir default-directory))
                       (expand-file-name advent-dir)))

(defun advent--infer-from-path (dir)
  "Return (YEAR DAY) list if DIR path ends with /YYYY/DD/."
  (let ((re "/\\(20[0-9][0-9]\\)/\\([0-9][0-9]?\\)/"))
    (when (string-match re (file-name-as-directory (expand-file-name dir)))
      (list (match-string 1 dir)
            (number-to-string (string-to-number (match-string 2 dir)))))))

(defun advent--infer-context ()
  "Infer (YEAR DAY) from current path or default to today's date."
  (or (advent--infer-from-path
       (or (and buffer-file-name (file-name-directory buffer-file-name))
           default-directory))
      (list (format-time-string "%Y")
            (number-to-string (advent--day)))))

(defun advent--refresh-mode-lines ()
  "Force a mode-line refresh in all buffers where `advent-mode' is enabled."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p advent-mode)
        (force-mode-line-update t)))))

(defun advent--mode-line ()
  "Return formatted AoC mode line indicator."
  (pcase-let* ((`(,year ,day) (advent--infer-context))
               (cookie (advent--cookie-status)))
    (format advent-mode-line-format year day cookie)))

;;;; Minor modes

;;;###autoload
(define-minor-mode advent-mode
  "Display AoC year/day and cookie status in the mode line."
  :lighter nil
  (if advent-mode
      (unless (assq 'advent-mode minor-mode-alist)
        (push '(advent-mode (:eval (advent--mode-line)))
              minor-mode-alist))
    (setq minor-mode-alist
          (assq-delete-all 'advent-mode minor-mode-alist)))
  (force-mode-line-update))

(defun advent--maybe-enable ()
  "Enable `advent-mode' in AoC project directories."
  (when (advent--in-aoc-project-p)
    (advent-mode 1)))

;;;###autoload
(define-minor-mode global-advent-mode
  "Automatically enable `advent-mode' in Advent of Code projects."
  :global t
  (if global-advent-mode
      (progn
        (add-hook 'find-file-hook #'advent--maybe-enable)
        (add-hook 'dired-mode-hook #'advent--maybe-enable)
        (advent--maybe-enable))
    (remove-hook 'find-file-hook #'advent--maybe-enable)
    (remove-hook 'dired-mode-hook #'advent--maybe-enable)))

(provide 'advent)
;;; advent.el ends here
