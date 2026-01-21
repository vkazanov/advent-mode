;;; advent-mode.el --- Advent of Code helper minor mode -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Keegan Carruthers-Smith
;; Copyright (C) 2026  Vladimir Kazanov

;; Author: Vladimir Kazanov
;; Keywords: lisp
;; Maintainer: Vladimir Kazanov
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/vkazanov/advent-mode
;; Version: 0.1

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

;; Advent of Code helper minor mode.

;; Features:
;;
;; - Global minor mode that autodetects AoC code.
;;
;; - Mode-line shows year/day/cookie
;;
;; - Assumed directory layout: path/to/aoc/year<YYYY>/day<DD>
;;
;; - Commands to open problem, fetch input, add new day, submit
;;   answers.
;;
;; Inspired by https://github.com/keegancsmith/advent/

;;; Code:

(require 'url)
(require 'url-cookie)
(require 'eww)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(defgroup advent nil
  "Advent of Code helpers."
  :group 'convenience)

(defcustom advent-root-dir nil
  "Root directory containing Advent of Code solutions."
  :type 'directory
  :group 'advent)

(defcustom advent-year-dir-format "year%04d"
  "Format string for the year directory, used with `format'."
  :type 'string
  :group 'advent)

(defcustom advent-day-dir-format "day%02d"
  "Format string for the day directory, used with `format'."
  :type 'string
  :group 'advent)

(defcustom advent-problem-path-re
  "^\\(?:year\\)?\\([0-9]\\{4\\}\\)/\\(?:day\\)?\\([0-9]\\{1,2\\}\\)/"
  "Regexp for the full problem dir path.
Subexpr 1 should be a problem's year.
Subexpr 2 should be a problem's day"
  :type 'regexp
  :group 'advent)

(defcustom advent-input-file-name "input.txt"
  "Name of the input file saved under a day directory."
  :type 'string
  :group 'advent)

(defcustom advent-new-files nil
  "List of files to copy into new day directories.
Absolute paths are copied as-is, relative paths are resolved from
`advent-root-dir'."
  :type '(repeat file)
  :group 'advent)

(defcustom advent-mode-line-format " AoC[%s/%s %s]"
  "Mode line format.  Receives (YEAR DAY COOKIE-STATUS)."
  :type 'string
  :group 'advent)

(defcustom advent-timezone "America/New_York"
  "Timezone used when computing AoC default year/day."
  :type 'string
  :group 'advent)

(defvar advent-submit-level-history nil)

;;;; Helper functions.

(defun advent--default-aoc-year-day (time)
  "Return (YEAR DAY) default AoC puzzle for TIME.
Use `advent-timezone'.  If TIME is in December (AoC timezone) then use
that YEAR and DAY=min(day-of-month, 25).  Otherwise use previous YEAR
and DAY=25."
  (pcase-let ((`(,_sec ,_min ,_hour ,dom ,mon ,year ,_dow ,_dst ,_tz)
               (decode-time (or time (current-time)) advent-timezone)))
    (if (= mon 12)
        (list year (min dom 25))
      (list (1- year) 25))))

(defun advent--problem-dir (year day root)
  "YEAR/DAY problem directory path under ROOT."
  (file-name-concat root
                    (format advent-year-dir-format year)
                    (format advent-day-dir-format day)))

(defun advent--input-path (year day root)
  "YEAR/DAY input file path under ROOT."
  (file-name-concat (advent--problem-dir year day root)
                    advent-input-file-name))

(defun advent--problem-url (year day)
  "YEAR/DAY problem url."
  (format "https://adventofcode.com/%d/day/%d" year day))

(defun advent--input-url (year day)
  "YEAR/DAY input url."
  (format "https://adventofcode.com/%d/day/%d/input" year day))

(defun advent--answer-url (year day)
  "YEAR/DAY answer url."
  (format "https://adventofcode.com/%d/day/%d/answer" year day))

(defun advent--normalize-dir (dir)
  "Return DIR normalized as a directory."
  (file-name-as-directory (expand-file-name dir)))

(defun advent--root ()
  "Return expanded `advent-root-dir' as a directory name, or nil."
  (when advent-root-dir (advent--normalize-dir advent-root-dir)))

(defun advent--current-buffer-dir ()
  "Return the path of the current buffer file, or `default-directory'."
  (or
   ;; prefer buffer file name
   (and buffer-file-name (file-name-directory buffer-file-name))
   ;; but a directory would do (like in non-file buffers)
   default-directory))

(defun advent--relative-dir (dir)
  "Return normalized DIR path relative to AoC root, or nil."
  (when-let ((root (advent--root)))
    (let ((abs (advent--normalize-dir dir)))
      (when (file-in-directory-p abs root)
        (file-name-as-directory (file-relative-name abs root))))))

(defun advent--infer-year-day-from-path (path)
  "Infer (YEAR DAY) from PATH.
PATH is expected to be relative to `advent-root-dir'."
  (when (string-match advent-problem-path-re path)
    (list (string-to-number (match-string 1 path))
          (string-to-number (match-string 2 path)))))

(defun advent--context-year-day ()
  "Infer (YEAR DAY) from the current buffer location in `advent-root-dir'.
Return nil if not in the root dir."
  (when-let ((rel (advent--relative-dir (advent--current-buffer-dir))))
    (advent--infer-year-day-from-path rel)))

(defun advent--ensure-context-or-error (year day)
  "Return (YEAR DAY) from explicit args or context.
Signal `user-error' otherwise."
  (let* ((ctx (advent--context-year-day))
         (y (or year (car ctx)))
         (d (or day (cadr ctx))))
    (unless (and y d) (user-error "Problem not detected"))
    (list y d)))

(defun advent--default-answer ()
  "Return default answer from region or thing at point."
  (string-trim
   (format "%s"
           (or (and (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))
               (thing-at-point 'number t)
               (thing-at-point 'symbol t)
               (thing-at-point 'line t)
               ""))))

;;;; Cookie management

(defun advent--ensure-cookie-or-error ()
  "Check if cookie is set.
Suggest setting the cookie, error otherwise."
  (unless (advent--cookie-ok-p)
    (if (y-or-n-p "AoC session cookie missing.  Set it now? ")
        (call-interactively #'advent-login)
      (user-error "No AoC session cookie set; run M-x advent-login"))))

(defun advent--cookie-ok-p ()
  "Non-nil if a non-expired AoC `session' cookie exists."
  (when-let* ((cookies (url-cookie-retrieve ".adventofcode.com" "/" t))
              (c (seq-find (lambda (cc) (string= (url-cookie-name cc) "session"))
                           cookies)))
    (not (url-cookie-expired-p c))))

(defun advent--cookie-status-string ()
  "Return a string representing cookie status."
  (if (advent--cookie-ok-p) "✓" "✗"))

(defun advent--refresh-mode-lines ()
  "Update mode-lines of `advent-mode' buffers."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (bound-and-true-p advent-mode)
        (force-mode-line-update t)))))

;;;###autoload
(defun advent-login (session)
  "Login to AoC by providing the SESSION cookie."
  (interactive "sSession cookie: ")
  (url-cookie-store "session" session
                    "Fri, 25 Dec 2031 00:00:00 GMT"
                    ".adventofcode.com" "/" t)
  (advent--refresh-mode-lines)
  (message "AoC session cookie stored."))

;;;; IO helpers

(defun advent--maybe-create-dir (dir)
  "Create DIR if it doesn't exist.
Return non-nil if DIR was created."
  (unless (file-directory-p dir)
    (mkdir dir t)
    (message "Created %s" dir)
    t))

(defun advent--copy-templates (paths target root)
  "Copy PATHS to TARGET dir.
Relative PATHS are resolved relative to ROOT, absolute ones copied as
is."
  (dolist (f paths)
    (let* ((src (if (file-name-absolute-p f) f (expand-file-name f root)))
           (dst (file-name-concat target (file-name-nondirectory f))))
      (if (file-exists-p src)
          (copy-file src dst t)
        (warn "Template file not found: %s" src)))))

(defun advent--http--status ()
  "Return numeric HTTP status for current buffer, or nil."
  (or (and (boundp 'url-http-response-status) url-http-response-status)
      (save-excursion
        (goto-char (point-min))
        (when (looking-at "HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\)")
          (string-to-number (match-string 1))))))

(defun advent--http--body (require-nonempty)
  "Return response body from current buffer or signal an error.
REQUIRE-NONEMPTY - when t, error out if the body is empty."
  (let ((status (advent--http--status)))
    (unless status
      (error "Malformed HTTP response (no status)"))
    (goto-char (point-min))
    (unless (re-search-forward "\r?\n\r?\n" nil t)
      (error "Malformed HTTP response (no header/body separator)"))
    (let* ((body (buffer-substring-no-properties (point) (point-max)))
           (snippet (string-trim (substring body 0 (min 200 (length body))))))
      (when (>= status 400)
        (error "HTTP %d%s" status (if (string-empty-p snippet) "" (format ": %s" snippet))))
      (when (and require-nonempty (string-empty-p (string-trim body)))
        (error "Empty HTTP response body"))
      body)))

(defun advent--http-request (url &optional method data require-nonempty)
  "Synchronously request URL and return response body as string.
METHOD is \"GET\"/\"POST\"; DATA is urlencoded string for POST.
REQUIRE-NONEMPTY - when t, error out if the body is empty."
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers
          (when data '(("Content-Type" . "application/x-www-form-urlencoded"))))
         (url-request-data data)
         ;; Do not use the INHIBIT-COOKIE param in the call below!
         (buf (url-retrieve-synchronously url t nil 30)))
    (unless buf (error "Failed to %s %s" url-request-method url))
    (unwind-protect
        (with-current-buffer buf
          (advent--http--body require-nonempty))
      (kill-buffer buf))))

(defun advent--write-url-to-file (url file)
  "Synchronously GET URL and write body to FILE.
Return FILE or signal error."
  (let ((body (advent--http-request url "GET" nil t)))
    (advent--maybe-create-dir (file-name-directory file))
    (with-temp-file file (insert body))
    file))

(defun advent--http-post (url data)
  "Synchronously POST DATA (application/x-www-form-urlencoded) to URL.
Returns response body as string."
  (advent--http-request url "POST" data))

;;;; Commands

;;;###autoload
(defun advent-open-problem-page (&optional year day)
  "Open the AoC problem page for YEAR and DAY in EWW.
If not provided, infer from context or use AoC today."
  (interactive)
  (pcase-let ((`(,year ,day) (advent--ensure-context-or-error year day)))
    (advent--ensure-cookie-or-error)
    (eww-browse-url (advent--problem-url year day))))

;;;###autoload
(defun advent-open-input (&optional year day)
  "Fetch (if needed) and open the input for YEAR and DAY."
  (interactive)
  (pcase-let ((`(,year ,day) (advent--ensure-context-or-error year day)))
    (advent--ensure-cookie-or-error)
    (let ((dst (advent--input-path year day (advent--root))))
      (if (file-exists-p dst)
          (find-file-other-window dst)
        (advent--write-url-to-file (advent--input-url year day) dst)
        (message "%s saved." dst)
        (find-file-other-window dst)))))

;;;###autoload
(defun advent-submit-answer (answer level &optional year day)
  "Submit ANSWER for LEVEL (1 or 2) for YEAR and DAY.
Return server response."
  (interactive
   (let* ((def (advent--default-answer))
          (ans (read-string "Answer: " def nil))
          (lvl (completing-read "Level: " '("1" "2") nil t nil 'advent-submit-level-history "1")))
     (list ans lvl nil nil)))
  (pcase-let ((`(,year ,day) (advent--ensure-context-or-error year day)))
    (advent--ensure-cookie-or-error)
    (let ((resp (advent--http-post (advent--answer-url year day)
                                   (format "level=%s&answer=%s"
                                           (url-hexify-string (format "%s" level))
                                           (url-hexify-string (format "%s" answer))))))
      (with-current-buffer (get-buffer-create "*AoC Submit*")
        (erase-buffer)
        (insert resp)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      (message "Submitted answer for %d day %d (level %s)" year day level)
      resp)))

;;;###autoload
(defun advent-open-day (year day &optional root)
  "Open YEAR/DAY problem directory under ROOT.
ROOT defaults to `advent-root-dir'.  Use default year/day as provided by
`advent--default-aoc-year-day'.  Create the directory if it doesn't
exist.  Suggest opening the problem page and retrieving the input."
  (interactive
   (pcase-let*
       ((`(,year-now ,day-now)
         (advent--default-aoc-year-day (current-time))))
     (list (read-number "Year: " year-now)
           (read-number "Day: "  day-now)
           nil)))
  (let* ((root (or root (advent--root)
                   (user-error "Variable advent-root-dir is not set")))
         (dir (advent--problem-dir year day root))
         (created (advent--maybe-create-dir dir)))
    (when (and created
               (y-or-n-p "Dir created.  Copy template files into it? "))
      (advent--copy-templates advent-new-files dir root))
    (dired dir)
    (when (y-or-n-p "Open the problem page in EWW? ")
      (advent-open-problem-page year day))
    (when (y-or-n-p (format "Download and open the input file? "))
      (advent-open-input year day))))

;;;; Mode line and modes

(defun advent--mode-line (&optional year day)
  "Generate a mode line using current directory using CTX.
CTX is a (YEAR DAY) pair either inferred or submitted."
  (let* ((ctx (advent--context-year-day))
         (year (or year (car ctx)))
         (day (or day (cadr ctx))))
    (format advent-mode-line-format
            year
            (format "%02d" day)
            (advent--cookie-status-string))))

(defvar-keymap advent-mode-map
  :doc "Keymap for `advent-mode'."
  "C-c a p" #'advent-open-problem-page
  "C-c a i" #'advent-open-input
  "C-c a s" #'advent-submit-answer
  "C-c a d" #'advent-open-day)

;;;###autoload
(define-minor-mode advent-mode
  "Show AoC year/day and cookie status in the mode line."
  :keymap advent-mode-map
  :lighter (:eval (advent--mode-line))
  (force-mode-line-update))

(defun advent--maybe-enable ()
  "Enable `advent-mode' in the current buffer if possible."
  (when (advent--relative-dir (advent--current-buffer-dir))
    (advent-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-advent-mode
  advent-mode advent--maybe-enable
  :group 'advent)

(provide 'advent-mode)
;;; advent-mode.el ends here
