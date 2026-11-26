;;; advent-mode.el --- Advent of Code mode -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2021 Keegan Carruthers-Smith
;; Copyright (C) 2026  Vladimir Kazanov

;; Author: Vladimir Kazanov <@vkazanov>
;; Keywords: tools
;; Maintainer: Vladimir Kazanov <@vkazanov>
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/vkazanov/advent-mode
;; Version: 0.2

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
  "Format string for the year, day and cookie status in the mode line.
Use %s for the year and day to display \"xxxx\" and \"xx\" when unknown.
For a known puzzle, %s displays a two-digit day.  Numeric directives
such as %02d also work for known days."
  :type 'string
  :group 'advent)

(defcustom advent-timezone "America/New_York"
  "Timezone for puzzle prompts and the local date used by `advent-today'."
  :type 'string
  :group 'advent)

(defcustom advent-open-day-format "Y%04d/D%02d"
  "Puzzle label used in day completion, command feedback, and prompts.
The format string receives the year and day as integers."
  :type 'string
  :group 'advent)

(defvar advent-submit-level-history nil)

(defvar advent--login-label nil
  "Puzzle label during a login requested by a puzzle command.
Session providers are called without arguments.")

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
      (message "%s" (advent--puzzle-text advent--login-label "Session cookie found in auth-source"))
      (or (and (functionp secret) (funcall secret))
          (and (stringp secret) secret)))))

(defun advent-session-prompt ()
  "Prompt the user for an AoC session cookie string.
Surrounding whitespace is removed.  Signal `user-error' if nothing is entered."
  (let* ((prompt (advent--puzzle-text advent--login-label "Advent of Code session cookie: "))
         (session (string-trim (read-passwd prompt))))
    (when (string-empty-p session)
      (user-error "%s" (advent--puzzle-text advent--login-label "No session cookie entered")))
    session))

;;;; Helper functions.

(defun advent--calendar-year-day (time timezone)
  "Return the most recent puzzle date as of TIME in TIMEZONE.
Outside December, use the previous season's last day."
  (pcase-let ((`(,_sec ,_min ,_hour ,dom ,mon ,year ,_dow ,_dst ,_tz)
               (decode-time time timezone)))
    (if (= mon 12)
        (list year (min dom (advent--max-day year)))
      (list (1- year) (advent--max-day (1- year))))))

(defun advent--latest-release (time)
  "Return the inclusive (YEAR DAY) release bound at TIME, in UTC-5."
  (advent--calendar-year-day time (* -5 60 60)))

(defun advent--default-aoc-year-day (time &optional latest)
  "Return the default puzzle for TIME in `advent-timezone'.
Cap the default at LATEST, the release bound at TIME."
  (let ((local (advent--calendar-year-day time advent-timezone))
        (latest (or latest (advent--latest-release time))))
    (if (value< latest local) latest local)))

(defconst advent--first-year 2015
  "First year of Advent of Code.")

(defconst advent--first-12-day-year 2025
  "First year of Advent of Code with 12 puzzles instead of 25.")

(defun advent--max-day (year)
  "Return the maximum puzzle day for YEAR."
  (if (>= year advent--first-12-day-year) 12 25))

(defun advent--scheduled-year-day-p (year day)
  "Return non-nil when YEAR and DAY fit the AoC event calendar."
  (and (integerp year)
       (integerp day)
       (>= year advent--first-year)
       (<= 1 day (advent--max-day year))))

(defun advent--puzzle-on-date (time timezone)
  "Return (YEAR DAY) if a puzzle opens on the date of TIME in TIMEZONE."
  (let* ((date (decode-time time timezone))
         (year (decoded-time-year date))
         (day (decoded-time-day date)))
    (when (and (= (decoded-time-month date) 12)
               (advent--scheduled-year-day-p year day))
      (list year day))))

(defun advent--valid-year-day-p (year day latest)
  "Return non-nil when YEAR and DAY identify a puzzle released by LATEST."
  (and (advent--scheduled-year-day-p year day)
       (not (value< latest (list year day)))))

(defun advent--released-year-day (year day latest explain-locked)
  "Return (YEAR DAY) when released by LATEST, otherwise nil.
If EXPLAIN-LOCKED is non-nil, signal a user error for a scheduled
puzzle that has yet to unlock."
  (cond ((advent--valid-year-day-p year day latest) (list year day))
        ((and explain-locked (advent--scheduled-year-day-p year day))
         (user-error "%s opens on %04d-12-%02d at 05:00 UTC"
                     (advent--format-year-day year day) year day))))

(defun advent--ensure-valid-year-day-or-error (year day &optional latest)
  "Return (YEAR DAY) when released by LATEST, otherwise signal `user-error'.
LATEST defaults to the current release bound."
  (unless (and year day)
    (user-error "Supply both YEAR and DAY"))
  (or (advent--released-year-day year day (or latest (advent--latest-release (current-time))) t)
      (user-error "Invalid AoC year/day: year=%S day=%S (year >= %d, day 1..%s)"
                  year day advent--first-year
                  (if (integerp year) (advent--max-day year) "12 or 25"))))

(defun advent--explicit-year-day (year day)
  "Return nil if YEAR and DAY are both nil, otherwise validate the pair."
  (when (or year day)
    (advent--ensure-valid-year-day-or-error year day)))

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
  "Return the puzzle label for YEAR and DAY."
  (format advent-open-day-format year day))

(defun advent--puzzle-text (label format-string &rest args)
  "Format FORMAT-STRING with ARGS and prefix it with LABEL when non-nil."
  (let ((text (apply #'format format-string args)))
    (if label (concat label " - " text) text)))

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

(defun advent--root-or-error (&optional year day)
  "Return expanded `advent-root-dir' or signal an error for YEAR and DAY."
  (or (advent--root)
      (user-error "%s" (advent--puzzle-text
                        (and year day (advent--format-year-day year day))
                        "Set advent-root-dir to your Advent of Code solutions directory"))))

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

(defun advent--infer-year-day-from-path (path latest &optional explain-locked)
  "Infer a puzzle released by LATEST from PATH using configured dir formats.
PATH is relative to `advent-root-dir'.  EXPLAIN-LOCKED reports the
unlock time of a future puzzle."
  (let ((parts (split-string path "/" t)))
    (when (>= (length parts) 2)
      (let ((year (advent--parse-int-segment (car parts) advent-year-dir-format))
            (day (advent--parse-int-segment (cadr parts) advent-day-dir-format)))
        (advent--released-year-day year day latest explain-locked)))))

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

(defun advent--context-year-day (&optional latest explain-locked)
  "Infer (YEAR DAY) from the current buffer location in `advent-root-dir'.
LATEST defaults to the current release bound.  EXPLAIN-LOCKED reports
the unlock time if the directory identifies a future puzzle."
  (when-let* ((rel (advent--relative-dir (advent--current-buffer-dir))))
    (advent--infer-year-day-from-path
     rel (or latest (advent--latest-release (current-time))) explain-locked)))

(defun advent--ensure-context-or-error (&optional year day)
  "Return the explicit YEAR and DAY or infer both from the current buffer."
  (or (advent--explicit-year-day year day)
      (advent--context-year-day nil t)
      (user-error "Not inside an AoC day directory; use M-x advent-open-day first")))

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

(defun advent--ensure-answer-or-error (answer label)
  "Return ANSWER without surrounding whitespace.
Signal `user-error' for puzzle LABEL if nothing is left."
  (let ((trimmed (string-trim (format "%s" answer))))
    (when (string-empty-p trimmed)
      (user-error "%s" (advent--puzzle-text label "No answer given")))
    trimmed))

(defun advent--existing-days (root latest)
  "Return completion labels and puzzle dates under ROOT released by LATEST."
  (let ((root (advent--normalize-dir root))
        choices)
    (dolist (year-dir (directory-files root t directory-files-no-dot-files-regexp))
      (when (file-directory-p year-dir)
        (dolist (day-dir (directory-files year-dir t directory-files-no-dot-files-regexp))
          (when (file-directory-p day-dir)
            (when-let* ((rel (file-name-as-directory (file-relative-name day-dir root)))
                        (year-day (advent--infer-year-day-from-path rel latest)))
              (push (cons (apply #'advent--format-year-day year-day) year-day)
                    choices))))))
    (nreverse choices)))

(defun advent--read-existing-year-day (root &optional time)
  "Prompt for an existing AoC YEAR/DAY under ROOT.
Default tries `advent--default-aoc-year-day' with TIME."
  (let* ((time (or time (current-time)))
         (latest (advent--latest-release time))
         (choices (advent--existing-days root latest)))
    (unless choices
      (user-error "No released AoC day directories found under %s (use M-x advent-create-day)"
                  root))
    (pcase-let* ((`(,defy ,defd) (advent--default-aoc-year-day time latest))
                 (defkey (advent--format-year-day defy defd))
                 (default (or (assoc defkey choices) (car choices)))
                 (picked (completing-read "Open year/day: " choices
                                          nil t nil nil (car default))))
      (cdr (assoc picked choices)))))

;;;; Cookie management

(defun advent--ensure-cookie-or-error (label)
  "Check the cookie for puzzle LABEL and offer to log in if it is missing."
  ;; Commands can also run before `advent-mode' has been enabled.
  (url-do-setup)
  (unless (advent--cookie-ok-p)
    (if (y-or-n-p (advent--puzzle-text label "AoC session cookie missing. Set it now? "))
        (let ((advent--login-label label))
          (advent-login))
      (user-error "%s" (advent--puzzle-text label "No AoC session cookie set; run M-x advent-login")))))

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

;;;; IO helpers

(defun advent--maybe-create-dir (dir)
  "Create DIR if it doesn't exist.
Return non-nil if DIR was created."
  (unless (file-directory-p dir)
    (mkdir dir t)
    t))

(defun advent--copy-templates (paths target root label)
  "Copy PATHS to TARGET dir for puzzle LABEL.
Relative PATHS are resolved relative to ROOT, absolute ones copied as
is."
  (dolist (f paths)
    (let* ((src (if (file-name-absolute-p f) f (expand-file-name f root)))
           (dst (file-name-concat target (file-name-nondirectory f))))
      (if (file-exists-p src)
          (copy-file src dst t)
        (warn "%s" (advent--puzzle-text label "Template file not found: %s" src))))))

(defun advent--http--status ()
  "Return numeric HTTP status for current buffer, or nil."
  (or (and (boundp 'url-http-response-status) url-http-response-status)
      (save-excursion
        (goto-char (point-min))
        (when (looking-at "HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\)")
          (string-to-number (match-string 1))))))

(defun advent--http--body (label require-nonempty)
  "Return response body from current buffer or signal an error for LABEL.
REQUIRE-NONEMPTY - when t, error out if the body is empty."
  (let ((status (advent--http--status)))
    (unless status
      (error "%s" (advent--puzzle-text label "Malformed HTTP response (no status)")))
    (goto-char (point-min))
    (unless (re-search-forward "\r?\n\r?\n" nil t)
      (error "%s" (advent--puzzle-text label "Malformed HTTP response (no header/body separator)")))
    (let* ((body (buffer-substring-no-properties (point) (point-max)))
           (snippet (string-trim (substring body 0 (min 200 (length body))))))
      (when (>= status 400)
        (error "%s" (advent--puzzle-text label "HTTP %d%s" status
                                         (if (string-empty-p snippet) "" (concat ": " snippet)))))
      (when (and require-nonempty (string-empty-p (string-trim body)))
        (error "%s" (advent--puzzle-text label "Empty HTTP response body")))
      body)))

(defun advent--http-request (url label &optional method data require-nonempty)
  "Synchronously request URL for puzzle LABEL and return its response body.
METHOD is \"GET\"/\"POST\"; DATA is urlencoded string for POST.
REQUIRE-NONEMPTY - when t, error out if the body is empty."
  (let* ((url-request-method (or method "GET"))
         (url-request-extra-headers
          (when data '(("Content-Type" . "application/x-www-form-urlencoded"))))
         (url-request-data data)
         ;; Do not use the INHIBIT-COOKIE param in the call below!
         (buf (url-retrieve-synchronously url t nil 30)))
    (unless buf
      (error "%s" (advent--puzzle-text label "Failed to %s %s" url-request-method url)))
    (unwind-protect
        (with-current-buffer buf
          (advent--http--body label require-nonempty))
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

(defun advent--write-url-to-file (url file label)
  "Synchronously GET URL for puzzle LABEL and write its body to FILE.
Return FILE or signal error."
  (let ((body (advent--http-request url label "GET" nil t)))
    (advent--maybe-create-dir (file-name-directory file))
    (with-temp-file file (insert body))
    file))

(defun advent--http-post (url data label)
  "Synchronously POST form DATA to URL for puzzle LABEL.
Returns response body as string."
  (advent--http-request url label "POST" data))

(defun advent--prompt-year-day (time)
  "Prompt for AoC YEAR and DAY.
Default values come from `advent--default-aoc-year-day' with TIME."
  (pcase-let* ((latest (advent--latest-release time))
               (`(,default-year ,day) (advent--default-aoc-year-day time latest))
               (year (read-number "Year: " default-year))
               (default-day (when (advent--valid-year-day-p year 1 latest)
                              (min day (if (= year (car latest))
                                           (cadr latest)
                                         (advent--max-day year))))))
    (advent--ensure-valid-year-day-or-error
     year (read-number (format "Day for %s: " year) default-day))))

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
                     (user-error "%s" (advent--puzzle-text advent--login-label
                                                           "No session cookie available")))))
    ;; Run url.el's one-time setup first: it reads the cookie file and
    ;; would otherwise overwrite the cookie stored below.
    (url-do-setup)
    (url-cookie-store "session" session
                      "Fri, 25 Dec 2031 00:00:00 GMT"
                      ".adventofcode.com" "/" t))
  (force-mode-line-update t)
  (message "%s" (advent--puzzle-text advent--login-label "AoC session cookie stored.")))

;;;###autoload
(defun advent-login-reset ()
  "Forget the stored AoC session cookie.
Remove all adventofcode.com cookies from url.el storage and save the
cookie file.  Run `advent-login' to store a new one."
  (interactive)
  ;; As in `advent-login': setup would otherwise reload the cookie file.
  (url-do-setup)
  (url-cookie-delete-cookies (rx bos (? ".") "adventofcode.com" eos))
  (force-mode-line-update t)
  (message "AoC session cookie removed."))

;;;###autoload
(defun advent-browse-problem-page (&optional year day)
  "Open the AoC problem page for YEAR and DAY in EWW.
Supply YEAR and DAY together.  If both are nil, use the current buffer's
puzzle or prompt with defaults based on the date in `advent-timezone'.
With a prefix argument, always prompt for both."
  (interactive (when current-prefix-arg (advent--prompt-year-day (current-time))))
  (pcase-let ((`(,year ,day) (or (advent--explicit-year-day year day)
                                (advent--context-year-day nil t)
                                (advent--prompt-year-day (current-time)))))
    (eww-browse-url (advent--problem-url year day))))

;;;###autoload
(defun advent-fetch-input (&optional year day)
  "Open the input for YEAR and DAY, downloading it first if it is missing.
Supply YEAR and DAY together.  If both are nil, infer the puzzle from
the current buffer.  Downloading requires an AoC session cookie."
  (interactive)
  (pcase-let* ((explicit (advent--explicit-year-day year day))
               (root (advent--root-or-error year day))
               (`(,year ,day) (or explicit (advent--ensure-context-or-error)))
               (dst (advent--input-path year day root)))
    (unless (file-exists-p dst)
      (let ((label (advent--format-year-day year day)))
        (advent--ensure-cookie-or-error label)
        (advent--write-url-to-file (advent--input-url year day) dst label)
        (message "%s" (advent--puzzle-text label "Downloaded %s" advent-input-file-name))))
    (find-file-other-window dst)))

;;;###autoload
(defun advent-submit-answer (answer level &optional year day)
  "Submit ANSWER for LEVEL (1 or 2) for YEAR and DAY and return the response.
Supply YEAR and DAY together.  If both are nil, infer the puzzle from
the current buffer."
  (interactive
   ;; Check context before prompting, so a missing day fails immediately.
   (pcase-let* ((`(,year ,day) (advent--ensure-context-or-error))
                (label (advent--format-year-day year day))
                (def (advent--default-answer))
                (ans (advent--ensure-answer-or-error
                      (read-string (advent--puzzle-text label "Answer: ") def) label))
                (lvl (completing-read (advent--puzzle-text label "Level: ") '("1" "2") nil t nil
                                      'advent-submit-level-history "1")))
     (list ans lvl year day)))
  (pcase-let* ((`(,year ,day) (advent--ensure-context-or-error year day))
               (label (advent--format-year-day year day))
               (answer (advent--ensure-answer-or-error answer label)))
    (advent--ensure-cookie-or-error label)
    (let* ((url (advent--answer-url year day))
           (resp (advent--http-post
                  url
                  (format "level=%s&answer=%s"
                          (url-hexify-string (format "%s" level))
                          (url-hexify-string answer))
                  label)))
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
      (message "%s" (advent--puzzle-text label "Submitted answer for level %s" level))
      resp)))

;;;###autoload
(defun advent-open-day (&optional year day)
  "Open an existing YEAR/DAY problem directory under `advent-root-dir'.
Supply YEAR and DAY together.  If both are nil, prompt from the
existing days under that root."
  (interactive)
  (pcase-let* ((explicit (advent--explicit-year-day year day))
               (root (advent--root-or-error year day))
               (`(,year ,day) (or explicit (advent--read-existing-year-day root)))
               (dir (advent--problem-dir year day root)))
    (unless (file-directory-p dir)
      (user-error "%s" (advent--puzzle-text
                        (advent--format-year-day year day)
                        "Directory does not exist at %s. Use M-x advent-create-day to create it"
                        dir)))
    (dired dir)))

;;;###autoload
(defun advent-create-day (year day)
  "Create YEAR/DAY directory under `advent-root-dir' and optionally init it.
Prompt for both YEAR and DAY interactively.  Both are required from Lisp.
If the directory exists, offer to open it with `advent-open-day'.

When a new directory is created, optionally copy `advent-new-files' into
it, then offer to open the problem page and download/open the input
file."
  (interactive (advent--prompt-year-day (current-time)))
  (pcase-let* ((`(,year ,day) (advent--ensure-valid-year-day-or-error year day))
               (root (advent--root-or-error year day))
               (label-format advent-open-day-format)
               (label (advent--format-year-day year day))
               (dir (advent--problem-dir year day root)))
    (if (not (advent--maybe-create-dir dir))
        (when (y-or-n-p (advent--puzzle-text label "Directory already exists. Open it? "))
          (advent-open-day year day))
      (message "%s" (advent--puzzle-text label "Created solution directory"))
      (when (and advent-new-files
                 (y-or-n-p (advent--puzzle-text label "Copy template files here? ")))
        (advent--copy-templates advent-new-files dir root label))
      (dired dir)
      (when (y-or-n-p (advent--puzzle-text label "Open the problem page in EWW? "))
        (advent-browse-problem-page year day))
      (when (y-or-n-p (advent--puzzle-text label "Download and open the input file? "))
        ;; Dired or EWW may have its own buffer-local puzzle format.
        (let ((advent-open-day-format label-format))
          (advent-fetch-input year day))))))

;;;###autoload
(defun advent-today (&optional year day)
  "Visit the puzzle for YEAR and DAY.
Supply YEAR and DAY together.  If both are nil, visit the puzzle opening
today in `advent-timezone', or signal `user-error' outside the event.
With a prefix argument, prompt for both.

For a new day, copy `advent-new-files' and open the problem in EWW
alongside Dired.

For an existing day, open its directory in Dired."
  (interactive (when current-prefix-arg (advent--prompt-year-day (current-time))))
  (pcase-let* ((now (current-time))
               (pair (if (or year day) (list year day)
                       (or (advent--puzzle-on-date now advent-timezone)
                           (user-error (concat "No Advent of Code puzzle opens on %s. "
                                               "Use C-u M-x advent-today to choose a year and day")
                                       (format-time-string "%B %-d" now advent-timezone)))))
               (`(,year ,day) (advent--ensure-valid-year-day-or-error
                               (car pair) (cadr pair) (advent--latest-release now)))
               (root (advent--root-or-error year day))
               (dir (advent--problem-dir year day root))
               (created (advent--maybe-create-dir dir)))
    (when created
      (let ((label (advent--format-year-day year day)))
        (message "%s" (advent--puzzle-text label "Created solution directory"))
        (advent--copy-templates advent-new-files dir root label)))
    (dired dir)
    (when created
      ;; Leave Dired visible while EWW takes over the other window.
      (switch-to-buffer-other-window (current-buffer))
      (advent-browse-problem-page year day))))

;;;; Mode line and modes

(defvar-local advent--mode-line-cache nil
  "Cons of the last lookup inputs and the inferred (YEAR DAY), possibly nil.")

(defun advent--refresh-mode-line-context (&optional _window)
  "Refresh the current buffer's cached puzzle when its lookup inputs change."
  (let* ((latest (advent--latest-release (current-time)))
         (inputs (list buffer-file-name default-directory advent-root-dir
                       advent-year-dir-format advent-day-dir-format latest)))
    (unless (equal inputs (car advent--mode-line-cache))
      (setq advent--mode-line-cache (cons inputs (advent--context-year-day latest)))
      (force-mode-line-update))))

(defun advent--format-mode-line (format-string context cookie-status)
  "Apply FORMAT-STRING to puzzle CONTEXT and COOKIE-STATUS.
CONTEXT is (YEAR DAY) or nil for an unknown puzzle."
  (if (null context)
      (format format-string "xxxx" "xx" cookie-status)
    (pcase-let ((`(,year ,day) context))
      (condition-case nil
          (format format-string year (format "%02d" day) cookie-status)
        ;; A numeric day directive requires the original integer.
        (error (format format-string year day cookie-status))))))

(defun advent--mode-line ()
  "Render the current buffer's lighter from its cached puzzle."
  (advent--format-mode-line advent-mode-line-format (cdr advent--mode-line-cache)
                           (advent--cookie-status-string)))

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
  (if advent-mode
      (progn
        ;; Load saved cookies before the first mode-line render.
        (url-do-setup)
        (advent--refresh-mode-line-context)
        (add-hook 'pre-redisplay-functions #'advent--refresh-mode-line-context nil t))
    (remove-hook 'pre-redisplay-functions #'advent--refresh-mode-line-context t)
    (kill-local-variable 'advent--mode-line-cache)))

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
