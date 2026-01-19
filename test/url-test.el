;;; url-test.el --- Tests for Advent URL builders

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

;;; url-test.el ends here
