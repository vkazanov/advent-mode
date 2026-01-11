;;; advent-test.el --- ERT tests for advent.el  -*- lexical-binding: t; -*-

;; Running the tests:
;;   emacs -Q --batch -L . -L test \
;;     -l advent.el -l test/advent-test.el \
;;     -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

;;; Test helpers

(defun advent-test--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defmacro advent-test--with-temp-aoc (&rest body)
  "Run BODY with an isolated temporary AoC root and templates."
  (declare (indent 0))
  (let ((root (make-symbol "root"))
        (tpl  (make-symbol "tpl"))
        (lib  (make-symbol "lib")))
    `(let* ((,root (file-name-as-directory (make-temp-file "advent-test-" t)))
            (,tpl (expand-file-name "template.py" ,root))
            (,lib (expand-file-name "util.py" ,root)))
       (unwind-protect
           (let ((advent-dir ,root)
                 (advent-file-template ,tpl)
                 (advent-lib-template ,lib))
             ;; Create templates used by `advent-src`.
             (with-temp-file ,tpl (insert "# template\n"))
             (with-temp-file ,lib (insert "# util\n"))
             ,@body)
         ;; Best-effort cleanup
         (ignore-errors (delete-directory ,root t))))))

;;; Path inference

(ert-deftest advent-test-infer-from-path-matches ()
  (let ((dir "/tmp/x/2024/1/"))
    (should (equal (advent--infer-from-path dir) '("2024" "1"))))
  (let ((dir "/tmp/x/2020/09/"))
    (should (equal (advent--infer-from-path dir) '("2020" "9")))))

(ert-deftest advent-test-infer-from-path-nomatch ()
  (should (equal (advent--infer-from-path "/tmp/x/abcd/1/") nil))
  (should (equal (advent--infer-from-path "/tmp/x/1999/1/") nil))
  (should (equal (advent--infer-from-path "/tmp/x/2024/") nil)))

(ert-deftest advent-test-infer-context-prefers-buffer-file-name ()
  (advent-test--with-temp-aoc
    (let* ((y "2022") (d "7")
           (dir (file-name-as-directory (expand-file-name (format "%s/%s" y d) advent-dir)))
           (file (expand-file-name "part1.py" dir)))
      (make-directory dir t)
      (with-temp-file file (insert "# x\n"))
      (with-temp-buffer
        (setq default-directory "/tmp/")         ; should be ignored
        (setq buffer-file-name file)
        (should (equal (advent--infer-context) (list y d)))))))

(ert-deftest advent-test-infer-context-falls-back-to-today ()
  ;; Make the fallback deterministic by stubbing `format-time-string` and `advent--day`.
  (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2099"))
            ((symbol-function 'advent--day) (lambda () 13)))
    (with-temp-buffer
      (setq buffer-file-name nil)
      (setq default-directory "/tmp/")
      (should (equal (advent--infer-context) '("2099" "13"))))))

;;; Cookie status

(ert-deftest advent-test-cookie-ok-p-true ()
  (cl-letf (((symbol-function 'url-cookie-retrieve)
             (lambda (&rest _) (list 'cookie-object)))
            ((symbol-function 'url-cookie-expired-p)
             (lambda (_cookie) nil)))
    (should (advent--cookie-ok-p))
    (should (equal (advent--cookie-status) "✓"))))

(ert-deftest advent-test-cookie-ok-p-false ()
  (cl-letf (((symbol-function 'url-cookie-retrieve)
             (lambda (&rest _) (list 'cookie-object)))
            ((symbol-function 'url-cookie-expired-p)
             (lambda (_cookie) t)))
    (should-not (advent--cookie-ok-p))
    (should (equal (advent--cookie-status) "✗"))))

(ert-deftest advent-test-login-stores-cookie-and-refreshes ()
  (let ((stored nil)
        (refreshed nil))
    (cl-letf (((symbol-function 'url-cookie-store)
               (lambda (&rest args) (setq stored args)))
              ((symbol-function 'advent--refresh-mode-lines)
               (lambda () (setq refreshed t)))
              ;; silence `message`
              ((symbol-function 'message) (lambda (&rest _))))
      (advent-login "SESSIONVALUE")
      (should stored)
      ;; url-cookie-store signature we expect:
      ;; (name value expires domain path secure)
      (should (equal (nth 0 stored) "session"))
      (should (equal (nth 1 stored) "SESSIONVALUE"))
      (should refreshed))))

;;; Mode line

(ert-deftest advent-test-mode-line-formatting ()
  (let ((advent-mode-line-format " AoC[%s/%s %s]"))
    (cl-letf (((symbol-function 'advent--infer-context) (lambda () '("2019" "2")))
              ((symbol-function 'advent--cookie-status) (lambda () "✓")))
      (should (equal (advent--mode-line) " AoC[2019/2 ✓]")))))

;;; Download callback

(ert-deftest advent-test-download-callback-writes-body ()
  (let ((tmp (make-temp-file "advent-input-")))
    (unwind-protect
        (with-temp-buffer
          (insert "HTTP/1.1 200 OK\r\nX-Test: 1\r\n\r\nBODY\nLINE2\n")
          (advent--download-callback nil tmp)
          (should (file-exists-p tmp))
          (should (equal (advent-test--read-file tmp) "BODY\nLINE2\n")))
      (ignore-errors (delete-file tmp)))))

(ert-deftest advent-test-download-callback-error-does-not-write ()
  (let ((tmp (make-temp-file "advent-input-")))
    (unwind-protect
        (progn
          (delete-file tmp) ;; ensure it doesn't exist
          (with-temp-buffer
            (insert "HTTP/1.1 500 FAIL\r\n\r\nnope\n")
            ;; silence `message`
            (cl-letf (((symbol-function 'message) (lambda (&rest _))))
              (advent--download-callback '(:error (error . "boom")) tmp)))
          (should-not (file-exists-p tmp)))
      (ignore-errors (delete-file tmp)))))

;;; File creation: advent-src

(ert-deftest advent-test-advent-src-creates-files-from-templates ()
  (advent-test--with-temp-aoc
    (let* ((year "2021")
           (day  "3")
           (dir  (file-name-as-directory (expand-file-name (format "%s/%s" year day) advent-dir)))
           (file1 (expand-file-name "part1.py" dir))
           (file2 (expand-file-name "part2.py" dir))
           (util  (expand-file-name "util.py"  dir))
           (buf nil))
      (save-window-excursion
        (setq buf (advent-src year (string-to-number day))))
      (should (file-exists-p file1))
      (should (file-exists-p file2))
      (should (file-exists-p util))
      (should (string-match-p "template" (advent-test--read-file file1)))
      (should (string-match-p "template" (advent-test--read-file file2)))
      (should (string-match-p "util" (advent-test--read-file util)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Minor modes

(ert-deftest advent-test-advent-mode-alist-entry-added-and-removed ()
  (let ((before (assq 'advent-mode minor-mode-alist)))
    (unwind-protect
        (progn
          (advent-mode 1)
          (should (assq 'advent-mode minor-mode-alist))
          ;; Enable again should not duplicate the entry
          (advent-mode 1)
          (let ((count 0))
            (dolist (x minor-mode-alist)
              (when (eq (car-safe x) 'advent-mode)
                (setq count (1+ count))))
            (should (= count 1)))
          (advent-mode 0)
          (should-not (assq 'advent-mode minor-mode-alist)))
      ;; Restore prior state, if any
      (when before
        (add-to-list 'minor-mode-alist before)))))

(ert-deftest advent-test-maybe-enable-in-aoc-project ()
  (advent-test--with-temp-aoc
    (let ((default-directory (file-name-as-directory (expand-file-name "2024/1" advent-dir))))
      (make-directory default-directory t)
      (with-temp-buffer
        (setq default-directory default-directory)
        (advent-mode 0)
        (advent--maybe-enable)
        (should (bound-and-true-p advent-mode))))))

(ert-deftest advent-test-maybe-enable-not-in-aoc-project ()
  (advent-test--with-temp-aoc
    (let ((default-directory (file-name-as-directory (make-temp-file "not-aoc-" t))))
      (unwind-protect
          (with-temp-buffer
            (setq default-directory default-directory)
            (advent-mode 0)
            (advent--maybe-enable)
            (should-not (bound-and-true-p advent-mode)))
        (ignore-errors (delete-directory default-directory t))))))

(provide 'advent-test)
;;; advent-test.el ends here
