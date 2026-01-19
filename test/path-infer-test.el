;;; path-infer-tests.el --- Tests for advent--infer-year-day-from-path

(require 'ert)
(require 'advent-mode)

(ert-deftest advent-infer-year-day-immediate-with-prefixes ()
  "Accept year/day immediately under root with prefixes."
  (should (equal (advent--infer-year-day-from-path "year2024/day05/")
                 (list 2024 5)))
  (should (equal (advent--infer-year-day-from-path "year2024/day05/src/")
                 (list 2024 5))))

(ert-deftest advent-infer-year-day-immediate-without-prefixes ()
  "Accept numeric year/day immediately under root without prefixes."
  (should (equal (advent--infer-year-day-from-path "2024/05/")
                 (list 2024 5)))
  (should (equal (advent--infer-year-day-from-path "2024/05/src/")
                 (list 2024 5))))

(ert-deftest advent-infer-year-day-mixed-prefixes ()
  "Accept mixed cases where one component has a prefix."
  (should (equal (advent--infer-year-day-from-path "2024/day05/")
                 (list 2024 5)))
  (should (equal (advent--infer-year-day-from-path "year2024/05/")
                 (list 2024 5))))

(ert-deftest advent-infer-year-day-not-immediate-under-root ()
  "Reject when year/day are not the first two components."
  (should-not (advent--infer-year-day-from-path "src/2024/day05/"))
  (should-not (advent--infer-year-day-from-path "src/work/2024/day05/")))

(ert-deftest advent-infer-year-day-false-positives-are-rejected ()
  "Reject segments that are not clean year/day components."
  (should-not (advent--infer-year-day-from-path "foo2024/bar05/"))
  (should-not (advent--infer-year-day-from-path "2024/05x/")))

(provide 'path-infer-tests)
;;; path-infer-tests.el ends here
