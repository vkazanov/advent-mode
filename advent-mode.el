;;; advent-mode.el --- Advent of Code mode -*- lexical-binding: t; -*-

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

;; Advent of Code helper mode.
;;
;;  - Global minor mode autodetecting solution directories
;;
;;  - Display year/day/cookie in the mode line
;;
;;  - Manage the AoC session cookie
;;
;;  - Optionally persist the cookie in =.authinfo=
;;
;;  - Create a new AoC day (with boilerplate files)
;;
;;  - Browse existing day solutions
;;
;;  - Open the current problem using eww
;;
;;  - Fetch and open the puzzle input
;;
;;  - Submit an answer to AoC
;;
;; Inspired by https://github.com/keegancsmith/advent/

;;; Code:

(require 'url)
(require 'url-cookie)
(require 'eww)
(require 'auth-source)
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

(defcustom advent-mode-line-format " AoC[Y%s/D%s %s]"
  "Mode line format.  Receives (YEAR DAY COOKIE-STATUS)."
  :type 'string
  :group 'advent)

(defcustom advent-timezone "America/New_York"
  "Timezone used when computing AoC default year/day."
  :type 'string
  :group 'advent)

(defcustom advent-open-day-format "Y%04d/D%02d"
  "Open day completion format.  Receives (YEAR DAY)."
  :type 'string
  :group 'advent)

(defvar advent-submit-level-history nil)

(defcustom advent-session-provider #'advent-session-from-auth-source
  "Function used by `advent-login' to obtain the AoC session cookie.
The function is called with no arguments and should return the session
cookie string, or nil if unavailable.

See:
- `advent-session-from-auth-source'
- `advent-session-prompt'"
  :type 'function
  :group 'advent)

;;;; Session providers

(defun advent-session-from-auth-source ()
  "Return AoC session cookie from auth-source, or nil if not found."
  (when-let*
      ((entry (car (auth-source-search
                    :host "adventofcode.com"
                    :user "session"
                    :require '(:secret)
                    :max 1))))
    (let ((secret (plist-get entry :secret)))
      (or (and (functionp secret) (funcall secret))
          (and (stringp secret) secret)))))

(defun advent-session-prompt ()
  "Prompt the user for an AoC session cookie string."
  (read-string "Advent of Code session cookie: "))

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

(defun advent--format-year-day (year day)
  "Return display key for YEAR/DAY."
  (format advent-open-day-format year day))

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

(defun advent--existing-day-entries (root)
  "Return a list of (YEAR DAY DIR) for AoC days under ROOT.
DIR is an absolute directory name."
  (let ((root (advent--normalize-dir root))
        entries)
    (dolist (year-dir (directory-files root t directory-files-no-dot-files-regexp))
      (when (file-directory-p year-dir)
        (dolist (day-dir (directory-files year-dir t directory-files-no-dot-files-regexp))
          (when (file-directory-p day-dir)
            (when-let* ((rel (file-name-as-directory (file-relative-name day-dir root)))
                        (year-day (advent--infer-year-day-from-path rel)))
              (push (list (car year-day)
                          (cadr year-day)
                          (file-name-as-directory day-dir))
                    entries))))))
    entries))

(defun advent--existing-days (root)
  "Return an alist of existing AoC days under ROOT.
Each element is (DISPLAY . (YEAR DAY DIR))."
  (let (out)
    (dolist (entry (advent--existing-day-entries root))
      (pcase-let ((`(,year ,day ,_) entry))
        (push (cons (advent--format-year-day year day) entry) out)))
    out))

(defun advent--read-existing-year-day (root &optional time)
  "Prompt for an existing AoC YEAR/DAY under ROOT.
Default tries `advent--default-aoc-year-day' with TIME."
  (let* ((choices (advent--existing-days root)))
    (unless choices
      (user-error "No AoC day directories found under %s" root))
    (pcase-let* ((`(,defy ,defd) (advent--default-aoc-year-day time))
                 (defkey (advent--format-year-day defy defd))
                 (default (or (assoc defkey choices) (car choices)))
                 (picked (completing-read
                          "Open year/day: "
                          (mapcar #'car choices)
                          nil t nil nil (car default))))
      (pcase-let ((`(,y ,d ,dir) (cdr (assoc picked choices))))

        (list y d dir)))))

;;;; Cookie management

(defun advent--ensure-cookie-or-error ()
  "Check if cookie is set.
Suggest setting the cookie, error otherwise."
  (unless (advent--cookie-ok-p)
    (if (y-or-n-p "AoC session cookie missing.  Set it now? ")
        (advent-login nil)
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

(defun advent--cookie-get ()
  "Return AoC session cookie using `advent-session-provider', or nil."
  (and advent-session-provider (funcall advent-session-provider)))

(defun advent--refresh-mode-lines ()
  "Update mode-lines of `advent-mode' buffers."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (bound-and-true-p advent-mode)
        (force-mode-line-update t)))))

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

(defun advent--prompt-year-day (time)
  "Prompt for AoC YEAR and DAY.
Default values come from `advent--default-aoc-year-day' with TIME."
  (pcase-let ((`(,year ,day) (advent--default-aoc-year-day time)))
    (list (read-number "Year: " year)
          (read-number "Day: "  day))))

;;;; Commands

;;;###autoload
(defun advent-login (&optional session)
  "Login to AoC by storing the session cookie.
SESSION, when non-nil, is used directly.

When called interactively without a prefix arg, SESSION is obtained from
`advent-session-provider'.  If that returns nil, prompt the user.

When called interactively with a prefix arg, always prompt for SESSION."
  (interactive
   (list (when current-prefix-arg
           (advent-session-prompt))))
  (let ((session (or session
                     (advent--cookie-get)
                     (advent-session-prompt)
                     (user-error "No session cookie available"))))
    (url-cookie-store "session" session
                      "Fri, 25 Dec 2031 00:00:00 GMT"
                      ".adventofcode.com" "/" t))
  (advent--refresh-mode-lines)
  (message "AoC session cookie stored."))

;;;###autoload
(defun advent-browse-problem-page (&optional year day)
  "Open the AoC problem page for YEAR and DAY in EWW.
Try to infer year/day using the current directory or use AoC today.
Given a prefix arg, prompt for a YEAR and DAY."
  (interactive
   (if current-prefix-arg (advent--prompt-year-day (current-time))
     (list nil nil)))
  (pcase-let ((`(,year ,day)
               ;; Priority: args -> context -> prompt
               (or (and (and year day) (list year day))
                   (advent--context-year-day)
                   (advent--prompt-year-day (current-time)))))
    (eww-browse-url (advent--problem-url year day))))

;;;###autoload
(defun advent-fetch-input (&optional year day)
  "Fetch (if needed) and open the input for YEAR and DAY.
Infer year/day using the current directory."
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
Infer year/day using the current directory.  Return server response."
  (interactive
   (let* ((def (advent--default-answer))
          (ans (read-string "Answer: " def nil))
          (lvl (completing-read "Level: " '("1" "2") nil t nil 'advent-submit-level-history "1")))
     (list ans lvl nil nil)))
  (pcase-let ((`(,year ,day) (advent--ensure-context-or-error year day)))
    (advent--ensure-cookie-or-error)
    (let ((resp (advent--http-post
                 (advent--answer-url year day)
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
(defun advent-open-day (&optional year day root)
  "Open an existing YEAR/DAY problem directory under ROOT.
If YEAR/DAY are not provided (typical interactive use), prompt from
*existing* days under ROOT so this command works from anywhere as long
as `advent-root-dir' is set."
  (interactive)
  (let* ((root (or root (advent--root)
                   (user-error "Variable advent-root-dir is not set"))))
    (pcase-let* ((picked (unless (and year day)
                           (advent--read-existing-year-day root (current-time))))
                 (year (or year (nth 0 picked)))
                 (day  (or day  (nth 1 picked)))
                 (dir  (or (nth 2 picked)
                           (advent--problem-dir year day root))))
      (unless (file-directory-p dir)
        (user-error "Day dir does not exist: %s (use M-x advent-create-day)" dir))
      (dired dir))))

;;;###autoload
(defun advent-create-day (year day &optional root)
  "Create YEAR/DAY problem directory under ROOT and optionally initialize it.
ROOT defaults to `advent-root-dir'.  Create the directory if missing.
If it already exists, suggest opening it via `advent-open-day'.

When a new directory is created, optionally copy `advent-new-files' into
it, then offer to open the problem page and download/open the input
file."
  (interactive (advent--prompt-year-day (current-time)))
  (let* ((root (or root (advent--root)
                   (user-error "Variable advent-root-dir is not set")))
         (dir (advent--problem-dir year day root)))
    (if (file-directory-p dir)
        (when (y-or-n-p "Day dir already exists.  Open it? ")
          (advent-open-day year day root))
      (advent--maybe-create-dir dir)
      (when (and advent-new-files
                 (y-or-n-p "Dir created.  Copy template files into it? "))
        (advent--copy-templates advent-new-files dir root))
      (dired dir)
      (when (y-or-n-p "Open the problem page in EWW? ")
        (advent-browse-problem-page year day))
      (when (y-or-n-p "Download and open the input file? ")
        (advent-fetch-input year day)))))

;;;; Mode line and modes

(defun advent--mode-line (&optional year day)
  "Generate a mode line using using either YEAR/DAY or dir path.
Relative path used for YEAR/DAY inference works using
`advent--context-year-day'.  Provide a graceful fallback for the case
when a problem directory is not found."
  (let* ((ctx (advent--context-year-day))
         (year (or year (car ctx) "xxxx"))
         (day (or day (cadr ctx) "xx")))
    (format advent-mode-line-format
            year day
            (advent--cookie-status-string))))

(defvar-keymap advent-command-map
  :doc "Prefix keymap for Advent of Code commands."
  "p" #'advent-browse-problem-page
  "i" #'advent-fetch-input
  "s" #'advent-submit-answer
  "d" #'advent-open-day
  "c" #'advent-create-day)

(defvar-keymap advent-mode-map
  :doc "Keymap for `advent-mode'."
  "C-c a" advent-command-map)

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
