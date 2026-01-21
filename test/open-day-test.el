;;; advent-mode-open-day-tests.el --- Tests for advent-open-day  -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(defmacro advent-test--with-temp-root (&rest body)
  (declare (indent 0))
  `(let ((root (make-temp-file "advent-root-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory root t))))

(defun advent-test--expected-day-dir (root year day)
  (file-name-as-directory
   (advent--problem-dir year day (advent--normalize-dir root))))

(ert-deftest errors-when-root-missing ()
  "If neither explicit ROOT nor `advent-root-dir` is set, error cleanly."
  (let ((advent-root-dir nil))
    (with-temp-buffer
      (let ((default-directory "/tmp/"))
        (should-error (advent-open-day 2026 1 nil)
                      :type 'user-error)))))

(ert-deftest outside-root-when-root-explicit ()
  "Calling from outside AoC root should still create + dired the correct dir."
  (advent-test--with-temp-root
    (let* ((year 2026)
           (day 3)
           (advent-root-dir nil) ;; ensure explicit arg is used
           (default-directory "/tmp/")
           (opened nil))
      (cl-letf (((symbol-function #'y-or-n-p) (lambda (_prompt) nil))
                ((symbol-function #'dired) (lambda (dir) (setq opened dir) dir)))
        (advent-open-day year day root))
      (let ((expected (advent-test--expected-day-dir root year day)))
        (should (file-directory-p expected))
        (should (equal (file-name-as-directory opened) expected))))))

(ert-deftest do-not-offer-template-copy-if-dir-exists ()
  "Template-copy prompt should happen only on newly created directories."
  (advent-test--with-temp-root
    (let* ((year 2026)
           (day 4)
           (advent-root-dir nil)
           (default-directory "/tmp/")
           (expected (advent-test--expected-day-dir root year day))
           (prompts '()))
      (make-directory expected t) ;; pre-exist
      (cl-letf (((symbol-function #'y-or-n-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   nil))
                ((symbol-function #'dired) (lambda (_dir) nil)))
        (advent-open-day year day root))
      (should (seq-every-p (lambda (p)
                             (not (string-match-p "Copy template files" p)))
                           prompts)))))

(ert-deftest copy-templates-when-created-and-user-agrees ()
  "When directory is created and user agrees, template files get copied."
  (advent-test--with-temp-root
    (let* ((year 2026)
           (day 5)
           (advent-root-dir nil)
           (default-directory "/tmp/")
           (advent-new-files '("tmpl.txt"))
           (tmpl-src (expand-file-name "tmpl.txt" root))
           (expected-dir (advent-test--expected-day-dir root year day))
           (expected-file (expand-file-name "tmpl.txt" expected-dir)))
      (with-temp-file tmpl-src (insert "hello\n"))
      (cl-letf (((symbol-function #'y-or-n-p)
                 (lambda (prompt)
                   ;; Say YES only to the template-copy prompt; NO to the rest.
                   (string-match-p "Copy template files" prompt)))
                ((symbol-function #'dired) (lambda (_dir) nil))
                ((symbol-function #'advent-open-problem-page) (lambda (&rest _) (ert-fail "Should not be called")))
                ((symbol-function #'advent-open-input) (lambda (&rest _) (ert-fail "Should not be called"))))
        (advent-open-day year day root))
      (should (file-exists-p expected-file))
      (should (string= (with-temp-buffer
                         (insert-file-contents expected-file)
                         (buffer-string))
                       "hello\n")))))

(provide 'advent-mode-open-day-tests)
;;; advent-mode-open-day-tests.el ends here
