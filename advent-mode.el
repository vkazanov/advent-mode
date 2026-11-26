;;; advent-mode.el --- Advent of Code mode -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Keegan Carruthers-Smith
;; Copyright (C) 2026  Vladimir Kazanov

;; Author: Vladimir Kazanov <@vkazanov>
;; Keywords: tools
;; Maintainer: Vladimir Kazanov <@vkazanov>
;; Package-Requires: ((emacs "30.1"))
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
;;  - Optionally read the cookie from .authinfo
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
(require 'dom)
(require 'shr)
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
  "Return AoC session cookie using `auth-source', or nil if not found."
  (when-let*
      ((entry (car (auth-source-search
                    :host "adventofcode.com"
                    :user "session"
                    :require '(:secret)
                    :max 1))))
    (let ((secret (plist-get entry :secret)))
      (message "Session cookie found in auth-source")
      (or (and (functionp secret) (funcall secret))
          (and (stringp secret) secret)))))

(defun advent-session-prompt ()
  "Prompt the user for an AoC session cookie string.
Surrounding whitespace is removed.  Signal `user-error' if nothing is entered."
  (let ((session (string-trim (read-passwd "Advent of Code session cookie: "))))
    (when (string-empty-p session)
      (user-error "No session cookie entered"))
    session))

;;;; Helper functions.

(defun advent--default-aoc-year-day (time)
  "Return (YEAR DAY) default AoC puzzle for TIME.
Use `advent-timezone'.  If TIME is in December (AoC timezone) then use
that YEAR and DAY=min(day-of-month, `advent--max-day').  Otherwise use
the previous YEAR and its last day."
  (pcase-let ((`(,_sec ,_min ,_hour ,dom ,mon ,year ,_dow ,_dst ,_tz)
               (decode-time (or time (current-time)) advent-timezone)))
    (if (= mon 12)
        (list year (min dom (advent--max-day year)))
      (list (1- year) (advent--max-day (1- year))))))

(defconst advent--first-year 2015
  "First year of Advent of Code.")

(defconst advent--first-12-day-year 2025
  "First year of Advent of Code with 12 puzzles instead of 25.")

(defun advent--max-day (year)
  "Return the maximum puzzle day for YEAR."
  (if (>= year advent--first-12-day-year) 12 25))

(defun advent--puzzle-on-date (time timezone)
  "Return (YEAR DAY) if a puzzle opens on the date of TIME in TIMEZONE."
  (let* ((date (decode-time time timezone))
         (year (decoded-time-year date))
         (day (decoded-time-day date)))
    (when (and (>= year advent--first-year)
               (= (decoded-time-month date) 12)
               (<= day (advent--max-day year)))
      (list year day))))

(defun advent--current-year ()
  "Return the current calendar year."
  (decoded-time-year (decode-time nil advent-timezone)))

(defun advent--valid-year-day-p (year day)
  "Return non-nil when YEAR/DAY is a valid AoC problem coordinate."
  (and (integerp year)
       (integerp day)
       (>= year advent--first-year)
       (<= year (advent--current-year))
       (>= day 1)
       (<= day (advent--max-day year))))

(defun advent--ensure-valid-year-day-or-error (year day)
  "Return (YEAR DAY) when valid, otherwise signal `user-error'."
  (unless (advent--valid-year-day-p year day)
    (user-error "Invalid AoC year/day: year=%S day=%S (year %d..%d, day 1..%s)"
                year day advent--first-year (advent--current-year)
                (if (integerp year) (advent--max-day year) "12 or 25")))
  (list year day))

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

(defun advent--root-or-error ()
  "Return expanded `advent-root-dir', or signal `user-error' if unset."
  (or (advent--root)
      (user-error "Variable advent-root-dir is not set")))

(defun advent--current-buffer-dir ()
  "Return the path of the current buffer file, or `default-directory'."
  (or
   ;; prefer buffer file name
   (and buffer-file-name (file-name-directory buffer-file-name))
   ;; but a directory would do (like in non-file buffers)
   default-directory))

(defun advent--relative-dir (dir)
  "Return normalized DIR path relative to AoC root, or nil."
  (when-let* ((root (advent--root)))
    (let ((abs (advent--normalize-dir dir)))
      (when (file-in-directory-p abs root)
        (file-name-as-directory (file-relative-name abs root))))))

(defun advent--infer-year-day-from-path (path)
  "Infer (YEAR DAY) from PATH using configured dir formats.
PATH is expected to be relative to `advent-root-dir'."
  (let ((parts (split-string path "/" t)))
    (when (>= (length parts) 2)
      (let ((year (advent--parse-int-segment (car parts) advent-year-dir-format))
            (day (advent--parse-int-segment (cadr parts) advent-day-dir-format)))
        (when (and year day (advent--valid-year-day-p year day))
          (list year day))))))

(defun advent--parse-int-segment (segment format-string)
  "Parse integer from SEGMENT using FORMAT-STRING.
Return nil when SEGMENT does not match FORMAT-STRING."
  (when-let* ((parts (advent--split-int-format format-string)))
    (pcase-let ((`(,prefix ,suffix) parts))
      (let* ((segment-len (length segment))
             (prefix-len (length prefix))
             (suffix-len (length suffix))
             (digits-end (- segment-len suffix-len)))
        (when (and (string-prefix-p prefix segment)
                   (string-suffix-p suffix segment)
                   (> digits-end prefix-len))
          (let ((digits (substring segment prefix-len digits-end)))
            (when (string-match-p "\\`[0-9]+\\'" digits)
              (let ((n (string-to-number digits)))
                (when (string= segment (format format-string n))
                  n)))))))))

(defun advent--split-int-format (format-string)
  "Return (PREFIX SUFFIX) when FORMAT-STRING has a single integer directive."
  (when (string-match "\\`\\([^%]*\\)%[0-9]*d\\([^%]*\\)\\'" format-string)
    (list (match-string 1 format-string)
          (match-string 2 format-string))))

(defun advent--context-year-day ()
  "Infer (YEAR DAY) from the current buffer location in `advent-root-dir'.
Return nil if not in the root dir."
  (when-let* ((rel (advent--relative-dir (advent--current-buffer-dir))))
    (advent--infer-year-day-from-path rel)))

(defun advent--ensure-context-or-error (year day)
  "Return (YEAR DAY) from explicit args or context.
Signal `user-error' otherwise."
  (let* ((ctx (advent--context-year-day))
         (y (or year (car ctx)))
         (d (or day (cadr ctx))))
    (unless (and y d)
      (user-error "Not inside an AoC day directory; use M-x advent-open-day first"))
    (advent--ensure-valid-year-day-or-error y d)))

(defun advent--default-answer ()
  "Return default answer from the region or the number at point, or \"\".
The number is returned as written in the buffer, so leading zeros and
decimal digits survive."
  (let ((bounds (and (not (use-region-p))
                     (bounds-of-thing-at-point 'number))))
    (string-trim
     (cond ((use-region-p)
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (bounds
            (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (t "")))))

(defun advent--ensure-answer-or-error (answer)
  "Return ANSWER without surrounding whitespace.
Signal `user-error' if nothing is left."
  (let ((trimmed (string-trim (format "%s" answer))))
    (when (string-empty-p trimmed)
      (user-error "No answer given"))
    trimmed))

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
      (user-error "No AoC day directories found under %s (use M-x advent-create-day)"
                  root))
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
  "Return AoC session cookie using `advent-session-provider', or nil.
An empty string counts as no cookie."
  (when advent-session-provider
    (let ((session (funcall advent-session-provider)))
      (and (stringp session)
           (not (string-empty-p session))
           session))))

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

(defun advent--submit-article (html)
  "Return the <article> DOM node of the AoC response HTML, or nil.
Return nil when libxml is unavailable or the article has no text."
  (when (libxml-available-p)
    (when-let* ((dom (with-temp-buffer
                       (insert html)
                       (libxml-parse-html-region (point-min) (point-max))))
                (article (car (dom-by-tag dom 'article))))
      (unless (string-blank-p (string-join (dom-strings article)))
        article))))

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
    (advent--ensure-valid-year-day-or-error
     (read-number "Year: " year)
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
    ;; Run url.el's one-time setup first: it reads the cookie file and
    ;; would otherwise overwrite the cookie stored below.
    (url-do-setup)
    (url-cookie-store "session" session
                      "Fri, 25 Dec 2031 00:00:00 GMT"
                      ".adventofcode.com" "/" t))
  (advent--refresh-mode-lines)
  (message "AoC session cookie stored."))

;;;###autoload
(defun advent-login-reset ()
  "Forget the stored AoC session cookie.
Remove all adventofcode.com cookies from url.el storage and save the
cookie file.  Run `advent-login' to store a new one."
  (interactive)
  ;; As in `advent-login': setup would otherwise reload the cookie file.
  (url-do-setup)
  (url-cookie-delete-cookies (rx bos (? ".") "adventofcode.com" eos))
  (advent--refresh-mode-lines)
  (message "AoC session cookie removed."))

;;;###autoload
(defun advent-browse-problem-page (&optional year day)
  "Open the AoC problem page for YEAR and DAY in EWW.
Try to infer year/day using the current directory or use AoC today.
Given a prefix arg, prompt for a YEAR and DAY."
  (interactive
   (if current-prefix-arg (advent--prompt-year-day (current-time))
     (list nil nil)))
  (pcase-let* ((`(,year ,day)
                ;; Priority: args -> context -> prompt
                (or (and (and year day) (list year day))
                    (advent--context-year-day)
                    (advent--prompt-year-day (current-time))))
               (`(,year ,day) (advent--ensure-valid-year-day-or-error year day)))
    (eww-browse-url (advent--problem-url year day))))

;;;###autoload
(defun advent-fetch-input (&optional year day)
  "Open the input for YEAR and DAY, downloading it first if it is missing.
Downloading needs the AoC session cookie; opening an existing input does
not.  Infer year/day using the current directory."
  (interactive)
  (pcase-let* ((root (advent--root-or-error))
               (`(,year ,day) (advent--ensure-context-or-error year day))
               (dst (advent--input-path year day root)))
    (unless (file-exists-p dst)
      (advent--ensure-cookie-or-error)
      (advent--write-url-to-file (advent--input-url year day) dst)
      (message "%s saved." dst))
    (find-file-other-window dst)))

;;;###autoload
(defun advent-submit-answer (answer level &optional year day)
  "Submit ANSWER for LEVEL (1 or 2) for YEAR and DAY.
Infer year/day using the current directory.  Return server response."
  (interactive
   ;; Check context before prompting, so a missing day fails immediately.
   (pcase-let* ((`(,year ,day) (advent--ensure-context-or-error nil nil))
                (def (advent--default-answer))
                (ans (advent--ensure-answer-or-error
                      (read-string (format "Answer for %d day %02d: " year day)
                                   def nil)))
                (lvl (completing-read "Level: " '("1" "2") nil t nil
                                      'advent-submit-level-history "1")))
     (list ans lvl year day)))
  (pcase-let ((`(,year ,day) (advent--ensure-context-or-error year day)))
    (let ((answer (advent--ensure-answer-or-error answer)))
      (advent--ensure-cookie-or-error)
      (let* ((url (advent--answer-url year day))
             (resp (advent--http-post
                    url
                    (format "level=%s&answer=%s"
                            (url-hexify-string (format "%s" level))
                            (url-hexify-string answer)))))
        (with-current-buffer (get-buffer-create "*AoC Submit*")
          (special-mode)
          (setq-local browse-url-browser-function #'eww-browse-url)
          ;; Show only the verdict; fall back to the raw response.
          (let ((inhibit-read-only t)
                (article (advent--submit-article resp)))
            (erase-buffer)
            (if article
                (shr-insert-document
                 (dom-node 'html nil
                           (dom-node 'head nil (dom-node 'base `((href . ,url))))
                           (dom-node 'body nil article)))
              (insert resp)))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
        (message "Submitted answer for %d day %d (level %s)" year day level)
        resp))))

;;;###autoload
(defun advent-open-day (&optional year day)
  "Open an existing YEAR/DAY problem directory under `advent-root-dir'.
If YEAR/DAY are not provided (typical interactive use), prompt from
*existing* days so this command works from anywhere as long as
`advent-root-dir' is set."
  (interactive)
  (let* ((root (advent--root-or-error)))
    (pcase-let* ((picked (unless (and year day)
                           (advent--read-existing-year-day root (current-time))))
                 (year (or year (nth 0 picked)))
                 (day  (or day  (nth 1 picked)))
                 (`(,year ,day) (advent--ensure-valid-year-day-or-error year day))
                 (dir  (or (nth 2 picked)
                           (advent--problem-dir year day root))))
      (unless (file-directory-p dir)
        (user-error "Day dir does not exist: %s (use M-x advent-create-day)" dir))
      (dired dir))))

;;;###autoload
(defun advent-create-day (year day)
  "Create YEAR/DAY directory under `advent-root-dir' and optionally init it.
If it already exists, suggest opening it via `advent-open-day'.

When a new directory is created, optionally copy `advent-new-files' into
it, then offer to open the problem page and download/open the input
file."
  (interactive (advent--prompt-year-day (current-time)))
  (pcase-let* ((root (advent--root-or-error))
               (`(,year ,day) (advent--ensure-valid-year-day-or-error year day))
               (dir (advent--problem-dir year day root)))
    (if (file-directory-p dir)
        (when (y-or-n-p "Day dir already exists.  Open it? ")
          (advent-open-day year day))
      (advent--maybe-create-dir dir)
      (when (and advent-new-files
                 (y-or-n-p "Dir created.  Copy template files into it? "))
        (advent--copy-templates advent-new-files dir root))
      (dired dir)
      (when (y-or-n-p "Open the problem page in EWW? ")
        (advent-browse-problem-page year day))
      (when (y-or-n-p "Download and open the input file? ")
        (advent-fetch-input year day)))))

;;;###autoload
(defun advent-today (&optional year day)
  "Visit today's puzzle in `advent-timezone', or the specified YEAR and DAY.
With a prefix argument, prompt for year/day.

For a new day, copy `advent-new-files' and open the problem in EWW
alongside Dired.

For an existing day, open its directory in Dired."
  (interactive (when current-prefix-arg (advent--prompt-year-day (current-time))))
  (pcase-let* ((`(,year ,day)
                (if (or year day)
                    (advent--ensure-valid-year-day-or-error year day)
                  (or (advent--puzzle-on-date (current-time) advent-timezone)
                      (user-error "No Advent of Code puzzle today.  Use C-u M-x advent-today to choose a puzzle"))))
               (root (advent--root-or-error))
               (dir (advent--problem-dir year day root))
               (created (advent--maybe-create-dir dir)))
    (when created
      (advent--copy-templates advent-new-files dir root))
    (dired dir)
    (when created
      ;; Leave Dired visible while EWW takes over the other window.
      (switch-to-buffer-other-window (current-buffer))
      (advent-browse-problem-page year day))))

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
  :doc "Prefix keymap for Advent of Code commands that work anywhere."
  "l" #'advent-login
  "t" #'advent-today
  "d" #'advent-open-day
  "c" #'advent-create-day
  "p" #'advent-browse-problem-page)

(defvar-keymap advent-local-command-map
  :doc "Prefix keymap for Advent of Code commands in AoC buffers."
  :parent advent-command-map
  "i" #'advent-fetch-input
  "s" #'advent-submit-answer)

(defvar-keymap advent-global-mode-map
  :doc "Keymap for `global-advent-mode'.  See `advent-keymap-prefix'.")

(defvar-keymap advent-mode-map
  :doc "Keymap for `advent-mode'.  See `advent-keymap-prefix'.")

(defun advent--set-keymap-prefix (symbol prefix)
  "Set SYMBOL to PREFIX and bind the command maps under it.
The previous prefix, if any, is unbound first."
  (when-let* ((old (and (boundp symbol) (symbol-value symbol))))
    (keymap-unset advent-global-mode-map old t)
    (keymap-unset advent-mode-map old t))
  (when prefix
    (keymap-set advent-global-mode-map prefix advent-command-map)
    (keymap-set advent-mode-map prefix advent-local-command-map))
  (set-default-toplevel-value symbol prefix))

(defcustom advent-keymap-prefix nil
  "Prefix key for Advent of Code commands, or nil for none.
Binds `advent-command-map' while `global-advent-mode' is on and
`advent-local-command-map' in `advent-mode' buffers.  Set it with
`setopt' or Customize so that the bindings are updated."
  :type '(choice (const :tag "None" nil) key)
  :set #'advent--set-keymap-prefix
  :initialize #'custom-initialize-reset
  :group 'advent)

;;;###autoload
(define-minor-mode advent-mode
  "Show AoC year/day and cookie status in the mode line."
  :lighter (:eval (advent--mode-line))
  (force-mode-line-update))

(defun advent--maybe-enable ()
  "Enable `advent-mode' in the current buffer if possible."
  (when (advent--relative-dir (advent--current-buffer-dir))
    (advent-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-advent-mode
  advent-mode advent--maybe-enable
  :keymap advent-global-mode-map
  :group 'advent)

(provide 'advent-mode)
;;; advent-mode.el ends here
