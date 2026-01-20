;;; aoc-year-day-test.el --- Tests for default year/day
(require 'advent-mode)
(require 'ert)

(defun advent-test--time (year mon day hour min sec &optional zone)
  "Build an Emacs time value."
  (setq zone (or zone "America/New_York"))
  (encode-time sec min hour day mon year zone))

(ert-deftest default-aoc-year-day-december-in-range ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2026 12 1 0 0 0))
                 '(2026 1)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2026 12 15 12 0 0))
                 '(2026 15)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2026 12 25 23 59 59))
                 '(2026 25))))

(ert-deftest default-aoc-year-day-december-after-25 ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2026 12 26 0 0 0))
                 '(2026 25)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2026 12 31 23 59 59))
                 '(2026 25))))

(ert-deftest default-aoc-year-day-non-december ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2026 11 30 12 0 0))
                 '(2025 25)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2027 1 1 0 0 0))
                 '(2026 25))))

;;; aoc-year-day-test.el ends here
