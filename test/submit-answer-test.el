;;; submit-answer-test.el --- Guards against empty or padded answers  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ert-x)
(require 'advent-mode)

(defmacro advent-test--in-day (year day &rest body)
  "Run BODY with `default-directory' inside a temporary YEAR/DAY directory."
  (declare (indent 2))
  `(let* ((root (make-temp-file "aoc-root-" t))
          (advent-root-dir root)
          (daydir (expand-file-name (format "year%d/day%02d/" ,year ,day) root))
          (default-directory daydir)
          (buffer-file-name nil))
     (make-directory daydir t)
     ,@body))

(ert-deftest advent-submit-empty-answer-is-rejected ()
  "An empty answer signals `user-error' before any request is made."
  (advent-test--in-day 2024 5
    (cl-letf (((symbol-function 'advent--ensure-cookie-or-error)
               (lambda () (error "cookie checked")))
              ((symbol-function 'advent--http-post)
               (lambda (&rest _) (error "posted")))
              ((symbol-function 'display-buffer) #'ignore))
      (should (equal (error-message-string
                      (should-error (advent-submit-answer "   " "1" 2024 5)
                                    :type 'user-error))
                     "Y2024/D05 — No answer given")))))

(ert-deftest advent-submit-trims-answer ()
  "Surrounding whitespace is removed before the answer is posted."
  (advent-test--in-day 2024 5
    (let (posted)
      (cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
                ((symbol-function 'advent--http-post)
                 (lambda (_url data &rest _) (setq posted data) "ok"))
                ((symbol-function 'display-buffer) #'ignore))
        (advent-submit-answer "  42 " "1" 2024 5))
      (should (equal posted "level=1&answer=42")))))

(ert-deftest advent-submit-interactive-empty-answer-stops-before-level ()
  "An empty answer typed at the prompt is rejected before the level is asked."
  (advent-test--in-day 2024 5
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (error "level prompted"))))
      (should (equal (error-message-string
                      (should-error (call-interactively #'advent-submit-answer)
                                    :type 'user-error))
                     "Y2024/D05 — No answer given")))))

(ert-deftest advent-submit-interactive-prompt-names-target ()
  "Resolve the puzzle once for both the answer prompt and the submission."
  (let ((lookups 0) (advent-open-day-format "%d%%/%02d") prompts url)
    (cl-letf (((symbol-function 'advent--context-year-day)
               (lambda () (cl-incf lookups) '(2024 5)))
              ((symbol-function 'read-string)
               (lambda (p &rest _) (push p prompts) "42"))
              ((symbol-function 'completing-read)
               (lambda (p &rest _) (push p prompts) "1"))
              ((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
              ((symbol-function 'advent--http-post)
               (lambda (u _data &rest _) (setq url u) "ok"))
              ((symbol-function 'display-buffer) #'ignore))
      (ert-with-message-capture messages
        (call-interactively #'advent-submit-answer)
        (should (equal messages "2024%/05 — Submitted answer for level 1\n"))))
    (should (= lookups 1))
    (should (equal (nreverse prompts) '("2024%/05 — Answer: " "2024%/05 — Level: ")))
    (should (equal url (advent--answer-url 2024 5)))))

(ert-deftest advent-submit-response-buffer-is-read-only ()
  "The response buffer is read-only and is refreshed by each submission."
  (advent-test--in-day 2024 5
    (let ((responses '("first" "second")))
      (unwind-protect
          (cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
                    ((symbol-function 'advent--http-post)
                     (lambda (&rest _) (pop responses)))
                    ((symbol-function 'display-buffer) #'ignore))
            (advent-submit-answer "42" "1" 2024 5)
            (advent-submit-answer "43" "1" 2024 5)
            (with-current-buffer "*AoC Submit*"
              (should buffer-read-only)
              (should (equal (buffer-string) "second"))))
        (when (get-buffer "*AoC Submit*")
          (kill-buffer "*AoC Submit*"))))))

;;; Response rendering

(defconst advent-test--answer-page
  "<!DOCTYPE html>
<html lang=\"en-us\">
<head>
<meta charset=\"utf-8\"/>
<title>Day 5 - Advent of Code 2024</title>
<script>window.addEventListener('click', function(){});</script>
</head><!--
Oh, hello!  Funny seeing you here.
-->
<body>
<header><div><h1 class=\"title-global\"><a href=\"/\">Advent of Code</a></h1><nav><ul><li><a href=\"/2024/about\">[About]</a></li><li><a href=\"/2024/events\">[Events]</a></li></ul></nav></div></header>
<main>
<article><p>That&apos;s not the right answer; your answer is too high.  If you&apos;re stuck, make sure you&apos;re using the full input data.  Please wait one minute before trying again. <a href=\"/2024/day/5\">[Return to Day 5]</a></p></article>
</main>
</body>
</html>"
  "A wrong-answer page shaped like the one AoC returns.")

(ert-deftest advent-submit-article-extracts-the-verdict ()
  "Only the <article> of an answer page is kept."
  (let ((text (string-join (dom-strings (advent--submit-article advent-test--answer-page)))))
    (should (string-match-p "That's not the right answer" text))
    (should (string-match-p "\\[Return to Day 5\\]" text))
    (should-not (string-match-p "Advent of Code\\|\\[Events\\]" text))))

(ert-deftest advent-submit-article-nil-without-usable-article ()
  "Responses without an article with text yield nil."
  (dolist (html '("plain text" "" "<html><body><article>  </article></body></html>"))
    (should-not (advent--submit-article html))))

(defun advent-test--submit-response (resp)
  "Submit an answer with RESP as the server response; return the result.
Rendering uses `shr-use-fonts' nil, as batch Emacs has no window to fill."
  (cl-letf (((symbol-function 'advent--ensure-cookie-or-error) #'ignore)
            ((symbol-function 'advent--http-post) (lambda (&rest _) resp))
            ((symbol-function 'display-buffer) #'ignore))
    (let ((shr-use-fonts nil))
      (advent-submit-answer "42" "1" 2024 5))))

(ert-deftest advent-submit-renders-only-the-article ()
  "The response buffer shows the verdict with working links, not the page."
  (advent-test--in-day 2024 5
    (unwind-protect
        (progn
          (should (equal (advent-test--submit-response advent-test--answer-page)
                         advent-test--answer-page))
          (with-current-buffer "*AoC Submit*"
            (let ((text (string-clean-whitespace (buffer-string))))
              (should (string-match-p "That's not the right answer" text))
              (should-not (string-match-p "<\\|Advent of Code\\|\\[Events\\]" text)))
            (should buffer-read-only)
            (should (eq browse-url-browser-function #'eww-browse-url))
            (goto-char (point-min))
            (search-forward "[Return")
            (should (equal (get-text-property (point) 'shr-url)
                           "https://adventofcode.com/2024/day/5"))))
      (kill-buffer "*AoC Submit*"))))

(ert-deftest advent-submit-shows-raw-response-without-article ()
  "A response without an article is shown exactly as received."
  (advent-test--in-day 2024 5
    (unwind-protect
        (progn
          (advent-test--submit-response "Something unexpected")
          (with-current-buffer "*AoC Submit*"
            (should (equal (buffer-string) "Something unexpected"))))
      (kill-buffer "*AoC Submit*"))))

(provide 'submit-answer-test)
;;; submit-answer-test.el ends here
