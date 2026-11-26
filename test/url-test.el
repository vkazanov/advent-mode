;;; url-test.el --- Tests for Advent URL builders  -*- lexical-binding: t; -*-

(require 'advent-mode)
(require 'ert)

(ert-deftest url-builder-tests ()
  "Test URL builders for Advent of Code."
  (should (string= (advent--problem-url 2024 5)
                 "https://adventofcode.com/2024/day/5"))
  (should (string= (advent--input-url 2024 5)
                 "https://adventofcode.com/2024/day/5/input"))
  (should (string= (advent--answer-url 2024 5)
                 "https://adventofcode.com/2024/day/5/answer")))

(defun advent-test--call-with-url-setup (file-cookies fn)
  "Call FN with url.el's state isolated from the user's files.
Cookie and history files live in a temporary directory.  FILE-COOKIES
is a list of (NAME VALUE DOMAIN) entries written to the cookie file,
using url.el's own writer, before setup runs.  Timers are disabled so
none are left running."
  (let* ((dir (make-temp-file "advent-url-test" t))
         (url-cookie-file (expand-file-name "cookies" dir))
         (url-history-file (expand-file-name "history" dir))
         (url-cookie-save-interval nil)
         (url-history-save-interval nil)
         (url-cookie-timer nil)
         (url-history-timer nil)
         (url-setup-done nil)
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil))
    (unwind-protect
        (progn
          (when file-cookies
            (dolist (spec file-cookies)
              (url-cookie-store (nth 0 spec) (nth 1 spec)
                                "Fri, 25 Dec 2031 00:00:00 GMT"
                                (nth 2 spec) "/" t))
            (let ((url-cookies-changed-since-last-save t))
              (url-cookie-write-file url-cookie-file))
            (setq url-cookie-storage nil
                  url-cookie-secure-storage nil))
          (funcall fn))
      (delete-directory dir t))))

(ert-deftest advent-login-survives-url-setup ()
  "A login must survive url.el's lazy setup."
  (advent-test--call-with-url-setup
   '(("WMF-Uniq" "x" ".wikipedia.org"))
   (lambda ()
     (advent-login "test-session")
     ;; The first request triggers url.el's real setup.
     (url-do-setup)
     (should (advent--cookie-ok-p)))))

(ert-deftest advent-login-not-shadowed-by-stale-disk-cookie ()
  "A stale session cookie in the cookie file must not replace a login."
  (advent-test--call-with-url-setup
   '(("session" "stale" ".adventofcode.com"))
   (lambda ()
     (advent-login "test-session")
     (url-do-setup)
     (should (equal (mapcar #'url-cookie-value
                            (url-cookie-retrieve ".adventofcode.com" "/" t))
                    '("test-session"))))))

(ert-deftest advent-login-reset-survives-url-setup ()
  "A reset removes the session cookie from the cookie file, and only it."
  (advent-test--call-with-url-setup
   '(("session" "stale" ".adventofcode.com")
     ("WMF-Uniq" "x" ".wikipedia.org"))
   (lambda ()
     (advent-login-reset)
     ;; Reload url.el's state from the cookie file.
     (setq url-cookie-storage nil
           url-cookie-secure-storage nil
           url-setup-done nil)
     (url-do-setup)
     (should-not (advent--cookie-ok-p))
     (should (url-cookie-retrieve ".wikipedia.org" "/" t)))))

(provide 'url-test)
;;; url-test.el ends here
