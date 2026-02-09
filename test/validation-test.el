;;; validation-test.el --- YEAR/DAY validation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'advent-mode)

(ert-deftest advent-valid-year-day-p-accepts-valid-values ()
  (should (advent--valid-year-day-p 2015 1))
  (should (advent--valid-year-day-p 2024 25))
  (should (advent--valid-year-day-p 2025 12)))

(ert-deftest advent-valid-year-day-p-rejects-out-of-range-values ()
  (should-not (advent--valid-year-day-p 2014 1))
  (should-not (advent--valid-year-day-p 2024 0))
  (should-not (advent--valid-year-day-p 2024 26))
  (should-not (advent--valid-year-day-p -1 1))
  (should-not (advent--valid-year-day-p 2024 -2))
  ;; future year
  (should-not (advent--valid-year-day-p 9999 1))
  ;; 2025 had only 12 days
  (should-not (advent--valid-year-day-p 2025 13)))

(ert-deftest advent-ensure-context-or-error-rejects-invalid-explicit-values ()
  (should-error (advent--ensure-context-or-error 2014 1) :type 'user-error)
  (should-error (advent--ensure-context-or-error 2024 0) :type 'user-error)
  (should-error (advent--ensure-context-or-error 2024 26) :type 'user-error))

(ert-deftest advent-ensure-context-or-error-accepts-valid-explicit-values ()
  (should (equal (advent--ensure-context-or-error 2024 5) '(2024 5))))

(ert-deftest advent-browse-problem-page-rejects-invalid-explicit-values ()
  (should-error (advent-browse-problem-page 2024 26) :type 'user-error)
  (should-error (advent-browse-problem-page 2014 1) :type 'user-error))

(ert-deftest advent-open-day-rejects-invalid-explicit-values ()
  (should-error (advent-open-day 2024 0 "/tmp") :type 'user-error)
  (should-error (advent-open-day 2014 1 "/tmp") :type 'user-error))

(ert-deftest advent-create-day-rejects-invalid-explicit-values ()
  (should-error (advent-create-day 2024 26 "/tmp") :type 'user-error)
  (should-error (advent-create-day 2014 1 "/tmp") :type 'user-error))

;;; validation-test.el ends here
