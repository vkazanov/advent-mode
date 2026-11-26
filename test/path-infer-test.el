;;; path-infer-test.el --- Tests for advent--infer-year-day-from-path  -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(ert-deftest advent-infer-year-day-immediate-with-prefixes ()
  "Accept year/day immediately under root with prefixes."
  (should (equal (advent--infer-year-day-from-path "year2024/day05/" '(2025 12))
                 (list 2024 5)))
  (should (equal (advent--infer-year-day-from-path "year2024/day05/src/" '(2025 12))
                 (list 2024 5))))

(ert-deftest advent-infer-year-day-default-formats-reject-without-prefixes ()
  "Reject numeric year/day paths that do not match default formats."
  (should-not (advent--infer-year-day-from-path "2024/05/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "2024/05/src/" '(2025 12))))

(ert-deftest advent-infer-year-day-default-formats-reject-mixed-prefixes ()
  "Reject paths when one component does not match configured formats."
  (should-not (advent--infer-year-day-from-path "2024/day05/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "year2024/05/" '(2025 12))))

(ert-deftest advent-infer-year-day-not-immediate-under-root ()
  "Reject when year/day are not the first two components."
  (should-not (advent--infer-year-day-from-path "src/2024/day05/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "src/work/2024/day05/" '(2025 12))))

(ert-deftest advent-infer-year-day-false-positives-are-rejected ()
  "Reject segments that are not clean year/day components."
  (should-not (advent--infer-year-day-from-path "foo2024/bar05/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "2024/05x/" '(2025 12))))

(ert-deftest advent-infer-year-day-rejects-invalid-aoc-ranges ()
  "Reject inferred year/day that are out of AoC range."
  (should-not (advent--infer-year-day-from-path "year2024/day00/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "year2024/day26/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "year2014/day01/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "year9999/day01/" '(2025 12)))
  (should-not (advent--infer-year-day-from-path "year2025/day13/" '(2025 12))))

(ert-deftest advent-infer-year-day-uses-configured-dir-formats ()
  "Infer year/day from the first two components using dir formats."
  (let ((advent-year-dir-format "y%04d")
        (advent-day-dir-format "d%02d"))
    (should (equal (advent--infer-year-day-from-path "y2024/d05/" '(2025 12))
                   (list 2024 5)))
    (should (equal (advent--infer-year-day-from-path "y2024/d05/src/" '(2025 12))
                   (list 2024 5)))))

(ert-deftest advent-infer-year-day-custom-formats-reject-default-layout ()
  "Reject old default layout when configured formats are changed."
  (let ((advent-year-dir-format "y%04d")
        (advent-day-dir-format "d%02d"))
    (should-not (advent--infer-year-day-from-path "2024/05/src/" '(2025 12)))))

(provide 'path-infer-test)
;;; path-infer-test.el ends here
