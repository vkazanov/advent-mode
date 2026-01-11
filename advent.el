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
;; TODO: minor mode show year/day/*cookie status*
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
  "Path to the AoC solutions directory."
  :type 'directory
  :group 'advent)

(defcustom advent-file-template
  (file-name-concat (expand-file-name advent-dir) "template.py")
  "A template for the first AoC solution source of the day."
  :type 'file
  :group 'advent)

(defcustom advent-lib-template
  (file-name-concat (expand-file-name advent-dir) "util.py")
  "A template for the util code for the AoC solution of the day."
  :type 'file
  :group 'advent)

(defcustom advent-mode-line-format " AoC[%s/%s %s]"
  "Format string used in the mode line.
It receives (YEAR DAY COOKIE-STATUS)."
  :type 'string
  :group 'advent)

;;;; API

(defvar advent-submit-level-history nil
  "History of level submission.")

(defun advent-login (session)
  "Login to Advent of Code.
Argument SESSION - session cookie to use."
  (interactive "sValue of session cookie from logged in browser: ")
  (url-cookie-store "session" session "Thu, 25 Dec 2027 20:17:36 -0000" ".adventofcode.com" "/" t)
  (message "Cookie stored"))

(defun advent (prefix &optional year day)
  "Load todays adventofcode.com problem and input.
Non-nil PREFIX to submit YEAR and DAY manually.  Optional arguments
YEAR,DAY: Load this year,day instead.  Defaults to today."
  (interactive "P")
  (if prefix
      (progn
        (setq year (read-string "Year: " (format-time-string "%Y")))
        (setq day (read-number "Day: " (advent--day))))
    (setq year (or year (format-time-string "%Y")))
    (setq day (or day (advent--day))))
  ;; (delete-other-windows)
  (eww (format "https://adventofcode.com/%s/day/%d" year day))
  (advent-src year day)
  (advent-input year day)
  (switch-to-buffer "*eww*"))

(defun advent-submit (answer level &optional year day)
  "Submits ANSWER for LEVEL to todays adventofcode.com problem.
LEVEL - either 1 or 2.
YEAR - year to submit (default ot current)
DAY - day to submit (Defaults to today)."
  (interactive
   (list
    ;; answer
    (let ((answer-default (advent--default-answer)))
      (read-string
       (cond
        ((and answer-default (> (length answer-default) 0))
         (format "Submit (default %s): " answer-default))
        (t "Submit: "))
       nil nil answer-default))
    ;; level
    (let ((default-level (or (car advent-submit-level-history) "1")))
      (read-string (format "Level (%s): " default-level)
                   nil 'advent-submit-level-history default-level))))
  (let* ((year (or year (format-time-string "%Y")))
         (day (or day (advent--day)))
         (url (format "https://adventofcode.com/%s/day/%d/answer" year day))
         (url-request-method "POST")
         (url-request-data (format "level=%s&answer=%s" level answer))
         (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (eww-browse-url url)))

(defun advent-src (&optional year day)
  "Open a file for YEAR, DAY.
If non-existant, use `advent-file-template' to create one."
  (interactive "P")
  (let* ((year (or year (format-time-string "%Y")))
         (day (format "%d" (or day (advent--day))))
         (dir (file-name-concat (expand-file-name advent-dir) year day))
         (file1 (file-name-concat dir "part1.py"))
         (file2 (file-name-concat dir "part2.py")))
    (when (and (not (file-exists-p file1))
               (file-exists-p advent-file-template))
      (mkdir dir t)
      (copy-file advent-file-template file1)
      (copy-file advent-file-template file2)
      (copy-file advent-lib-template (concat dir "/")))
    (find-file file1)))

(defun advent-input (&optional year day)
  "Load adventofcode.com daily input.txt in other window.
Optional arguments YEAR/DAY: Load this day/year instead.  Defaults to
today."
  (interactive "P")
  (let* ((year (or year (format-time-string "%Y")))
         (day (format "%d" (or day (advent--day))))
         (url (format "https://adventofcode.com/%s/day/%s/input" year day))
         (dir (file-name-concat (expand-file-name advent-dir) year day))
         (file (file-name-concat dir "input.txt")))
    (if (not (file-exists-p file))
        (url-retrieve url 'advent--download-callback (list file))
      (find-file-other-window file))))

(defun advent--download-callback (status file)
  "Save the results retrieved to a specified FILE.
STATUS - request status."
  (if (plist-get status :error)
      (message "Failed to download todays advent %s" (plist-get status :error))
    (mkdir (file-name-directory file) t)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (write-region (point) (point-max) file)
    (find-file-other-window file)))

(defun advent--day ()
  "Return current day as a number based on the correct time zone."
  (elt (decode-time (current-time) "America/New_York") 3))

(defun advent--default-answer ()
  "Use current region as a default answer."
  (and transient-mark-mode mark-active
       (/= (point) (mark))
       (buffer-substring-no-properties (point) (mark))))

;;;; advent-mode

(defun advent--in-aoc-project-p (&optional dir)
  "Check if DIR is in the AoC solution rectory."
  (let ((dir (expand-file-name (or dir default-directory))))
    (file-in-directory-p dir (expand-file-name advent-dir))))

(defun advent--infer-from-path (dir)
  "Return (YEAR DAY) if DIR ends in .../YYYY/DD/."
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         ;; Match /YYYY/DD/ anywhere in the path
         (re (concat "/\\(20[0-9][0-9]\\)/\\([0-9][0-9]?\\)/")))
    (when (string-match re dir)
      (list (match-string 1 dir)
            (number-to-string (string-to-number (match-string 2 dir)))))))

(defun advent--infer-context ()
  "Return (YEAR DAY) based on path or today's date."
  (or (advent--infer-from-path (or (and buffer-file-name
                                       (file-name-directory buffer-file-name))
                                  default-directory))
      (list (format-time-string "%Y")
            (number-to-string (advent--day)))))

(defun advent--cookie-ok-p ()
  "Non-nil if AoC session cookie exists and is not expired."
  (let* ((cookies (url-cookie-retrieve ".adventofcode.com" "/" t)))
    (and (not (null cookies))
         (not (url-cookie-expired-p (car cookies))))))

(defun advent--cookie-status ()
  "Return symbol representing AoC session cookie status."
  (if (advent--cookie-ok-p) "✓" "✗"))

(defun advent--mode-line ()
  "AoC mode line string."
  (let* ((ctx (advent--infer-context))
         (year (car ctx))
         (day (cadr ctx))
         (cookie (advent--cookie-status)))
    (format advent-mode-line-format year day cookie)))

;;;###autoload
(define-minor-mode advent-mode
  "Show AoC year/day and cookie status in the mode line."
  :init-value nil
  :lighter nil
  (if advent-mode
      (unless (assq 'advent-mode minor-mode-alist)
        (push '(advent-mode (:eval (advent--mode-line))) minor-mode-alist))
    (setq minor-mode-alist
          (delq (assq 'advent-mode minor-mode-alist) minor-mode-alist)))
  (force-mode-line-update))

(defun advent--maybe-enable ()
  "Enable AoC mode when a solutions directory is detected."
  (when (advent--in-aoc-project-p default-directory)
    (advent-mode 1)))

;;;###autoload
(define-minor-mode global-advent-mode
  "Enable `advent-mode' automatically in AoC projects."
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
