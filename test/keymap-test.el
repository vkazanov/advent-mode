;;; keymap-test.el --- Tests for Advent key bindings  -*- lexical-binding: t; -*-

(require 'advent-mode)
(require 'ert)

(ert-deftest advent-command-map-bindings ()
  "All commands are reachable from `advent-command-map'."
  (should (eq (lookup-key advent-command-map "l") #'advent-login))
  (should (eq (lookup-key advent-command-map "p") #'advent-browse-problem-page))
  (should (eq (lookup-key advent-command-map "i") #'advent-fetch-input))
  (should (eq (lookup-key advent-command-map "s") #'advent-submit-answer))
  (should (eq (lookup-key advent-command-map "d") #'advent-open-day))
  (should (eq (lookup-key advent-command-map "c") #'advent-create-day)))

(provide 'keymap-test)
;;; keymap-test.el ends here
