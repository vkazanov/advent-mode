;;; context-test.el ---                              -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(defun advent-test--mkdir (dir)
  (make-directory dir t) dir)

(ert-deftest advent-relative-dir-nil-when-root-nil ()
  (let ((advent-root-dir nil))
    (let ((default-directory "/tmp/"))
      (should-not (advent--relative-dir "/tmp/something/dir"))
      (should-not (advent--context-year-day)))))

(ert-deftest advent-relative-dir-nil-when-outside-root ()
  (let* ((root (make-temp-file "aoc-root-" t))
         (advent-root-dir root)
         (other (make-temp-file "aoc-other-" t))
         (buffer-file-name (file-name-concat other "test/")))
    (let ((default-directory (file-name-as-directory other)))
      (should-not (advent--relative-dir buffer-file-name))
      (should-not (advent--context-year-day)))))

(ert-deftest advent-context-year-day-at-day-dir ()
  (let* ((root (make-temp-file "aoc-root-" t))
         (advent-root-dir root)
         (daydir (advent-test--mkdir
                  (expand-file-name "year2026/day01/" root))))
    (let ((default-directory (file-name-as-directory daydir))
          (buffer-file-name nil))
      (should (equal (advent--relative-dir daydir) "year2026/day01/"))
      (should (equal (advent--context-year-day) '(2026 1))))))

(ert-deftest advent-context-year-day-in-deeper-subdir ()
  (let* ((root (make-temp-file "aoc-root-" t))
         (advent-root-dir root)
         (deep (advent-test--mkdir
                (expand-file-name "year2026/day01/src/" root))))
    (let ((default-directory (file-name-as-directory deep))
          (buffer-file-name nil))
      (should (equal (advent--relative-dir deep) "year2026/day01/src/"))
      (should (equal (advent--context-year-day) '(2026 1))))))

(ert-deftest advent-context-year-day-prefers-buffer-file-name ()
  (let* ((root (make-temp-file "aoc-root-" t))
         (advent-root-dir root)
         (daydir (advent-test--mkdir
                  (expand-file-name "year2026/day02/" root)))
         (file (expand-file-name "solution.el" daydir)))
    ;; Create the file so paths are realistic (not strictly required for our pure funcs).
    (with-temp-file file (insert ";; hi\n"))
    (let ((default-directory "/tmp/")      ; intentionally unrelated
          (buffer-file-name file))
      (should (equal (advent--relative-dir daydir) "year2026/day02/"))
      (should (equal (advent--context-year-day) '(2026 2))))))

(ert-deftest advent-root-trailing-slash-does-not-matter ()
  (let* ((root (make-temp-file "aoc-root-" t))
         (daydir (advent-test--mkdir
                  (expand-file-name "year2026/day03/" root))))
    ;; No trailing slash
    (let ((advent-root-dir root)
          (default-directory (file-name-as-directory daydir))
          (buffer-file-name nil))
      (should (equal (advent--context-year-day) '(2026 3))))
    ;; With trailing slash
    (let ((advent-root-dir (file-name-as-directory root))
          (default-directory (file-name-as-directory daydir))
          (buffer-file-name nil))
      (should (equal (advent--context-year-day) '(2026 3))))))


;;; context-test.el ends here
