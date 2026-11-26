;;; aoc-year-day-test.el --- Tests for default year/day  -*- lexical-binding: t; -*-

(require 'advent-mode)
(require 'ert)

(defun advent-test--time (year mon day hour min sec &optional zone)
  "Build an Emacs time value."
  (setq zone (or zone "America/New_York"))
  (encode-time sec min hour day mon year zone))

(ert-deftest max-day-25-before-2025-then-12 ()
  (should (= (advent--max-day 2015) 25))
  (should (= (advent--max-day 2024) 25))
  (should (= (advent--max-day 2025) 12))
  (should (= (advent--max-day 2030) 12)))

(ert-deftest advent-release-boundaries ()
  (pcase-dolist (`(,year ,month ,day ,hour ,minute ,second ,latest)
                 '((2026 11 26 12 0 0 (2025 12))
                   (2026 12 1 4 59 59 (2025 12))
                   (2026 12 1 5 0 0 (2026 1))
                   (2026 12 2 4 59 59 (2026 1))
                   (2026 12 2 5 0 0 (2026 2))
                   (2026 12 12 5 0 0 (2026 12))
                   (2026 12 13 5 0 0 (2026 12))
                   (2024 12 25 5 0 0 (2024 25))
                   (2024 12 26 5 0 0 (2024 25))))
    (let ((time (advent-test--time year month day hour minute second t)))
      (should (equal (advent--latest-release time) latest)))))

(ert-deftest advent-prompt-default-follows-local-date-up-to-release ()
  (let ((time (advent-test--time 2026 12 1 4 59 59 t))
        (advent-timezone "Asia/Tokyo"))
    (should (equal (advent--default-aoc-year-day time) '(2025 12)))
    (setq time (time-add time 1))
    (should (equal (advent--default-aoc-year-day time) '(2026 1)))
    (let ((advent-timezone "America/Los_Angeles"))
      (should (equal (advent--default-aoc-year-day time) '(2025 12))))))

(ert-deftest advent-puzzle-on-date-follows-local-event-boundaries ()
  (pcase-dolist (`(,year ,month ,day ,hour ,minute ,second ,expected)
                 '((2026 12 1 4 59 59 nil)
                   (2026 12 1 5 0 0 (2026 1))
                   (2026 12 13 4 59 59 (2026 12))
                   (2026 12 13 5 0 0 nil)
                   (2024 12 25 5 0 0 (2024 25))
                   (2024 12 26 5 0 0 nil)
                   (2026 7 1 12 0 0 nil)
                   (2014 12 1 5 0 0 nil)))
    (let ((time (advent-test--time year month day hour minute second t)))
      (should (equal (advent--puzzle-on-date time "America/New_York") expected)))))

;;; 25-day years

(ert-deftest default-aoc-year-day-25-day-year-december-in-range ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2024 12 1 0 0 0))
                 '(2024 1)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2024 12 15 12 0 0))
                 '(2024 15)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2024 12 25 23 59 59))
                 '(2024 25))))

(ert-deftest default-aoc-year-day-25-day-year-december-after-25 ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2024 12 26 0 0 0))
                 '(2024 25)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2024 12 31 23 59 59))
                 '(2024 25))))

(ert-deftest default-aoc-year-day-non-december-after-25-day-year ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2025 1 1 0 0 0))
                 '(2024 25)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2025 11 30 12 0 0))
                 '(2024 25))))

;;; 12-day years

(ert-deftest default-aoc-year-day-12-day-year-december-in-range ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2030 12 1 0 0 0))
                 '(2030 1)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2030 12 12 23 59 59))
                 '(2030 12))))

(ert-deftest default-aoc-year-day-12-day-year-december-after-12 ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2030 12 13 0 0 0))
                 '(2030 12)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2030 12 31 23 59 59))
                 '(2030 12))))

(ert-deftest default-aoc-year-day-non-december-after-12-day-year ()
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2031 1 1 0 0 0))
                 '(2030 12)))
  (should (equal (advent--default-aoc-year-day
                  (advent-test--time 2031 11 30 12 0 0))
                 '(2030 12))))

(ert-deftest ensure-valid-year-day-non-integer-year-is-user-error ()
  (should-error (advent--ensure-valid-year-day-or-error nil 1)
                :type 'user-error)
  (should-error (advent--ensure-valid-year-day-or-error "2024" 1)
                :type 'user-error))

(provide 'aoc-year-day-test)
;;; aoc-year-day-test.el ends here
