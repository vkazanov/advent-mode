;;; keymap-test.el --- Tests for Advent key bindings  -*- lexical-binding: t; -*-

(require 'advent-mode)
(require 'ert)

(defconst advent-test--global-keys
  '(("l" . advent-login)
    ("d" . advent-open-day)
    ("c" . advent-create-day)
    ("p" . advent-browse-problem-page))
  "Commands that work anywhere.")

(defconst advent-test--local-keys
  '(("i" . advent-fetch-input)
    ("s" . advent-submit-answer))
  "Commands that work in AoC buffers only.")

(defmacro advent-test--with-prefix (prefix &rest body)
  "Run BODY with `advent-keymap-prefix' set to PREFIX, then reset it."
  (declare (indent 1))
  `(unwind-protect
       (progn (setopt advent-keymap-prefix ,prefix) ,@body)
     (setopt advent-keymap-prefix nil)))

(ert-deftest advent-command-map-has-global-commands-only ()
  "`advent-command-map' holds the commands that work anywhere."
  (pcase-dolist (`(,key . ,cmd) advent-test--global-keys)
    (should (eq (keymap-lookup advent-command-map key) cmd)))
  (pcase-dolist (`(,key . ,_) advent-test--local-keys)
    (should-not (keymap-lookup advent-command-map key))))

(ert-deftest advent-local-command-map-has-all-commands ()
  "`advent-local-command-map' adds local commands to the global ones."
  (pcase-dolist (`(,key . ,cmd) (append advent-test--global-keys
                                        advent-test--local-keys))
    (should (eq (keymap-lookup advent-local-command-map key) cmd))))

(ert-deftest advent-mode-maps-bind-nothing-without-prefix ()
  "No command is reachable unless `advent-keymap-prefix' is set."
  (should-not advent-keymap-prefix)
  (pcase-dolist (`(,_ . ,cmd) (append advent-test--global-keys
                                      advent-test--local-keys))
    (should-not (where-is-internal
                 cmd (list advent-global-mode-map advent-mode-map)))))

(ert-deftest advent-mode-maps-belong-to-their-modes ()
  "Each mode map is the one its mode activates."
  (should (eq (cdr (assq 'global-advent-mode minor-mode-map-alist))
              advent-global-mode-map))
  (should (eq (cdr (assq 'advent-mode minor-mode-map-alist))
              advent-mode-map)))

(ert-deftest advent-keymap-prefix-binds-both-mode-maps ()
  "Setting the prefix binds the command maps in both mode maps."
  (advent-test--with-prefix "C-c a"
    (should (eq (keymap-lookup advent-global-mode-map "C-c a")
                advent-command-map))
    (should (eq (keymap-lookup advent-mode-map "C-c a")
                advent-local-command-map))))

(ert-deftest advent-keymap-prefix-change-unbinds-old-prefix ()
  "Changing or clearing the prefix removes the old binding."
  (advent-test--with-prefix "C-c a"
    (setopt advent-keymap-prefix "C-c x")
    (should-not (keymap-lookup advent-global-mode-map "C-c a"))
    (should-not (keymap-lookup advent-mode-map "C-c a"))
    (should (keymap-lookup advent-global-mode-map "C-c x"))
    (setopt advent-keymap-prefix nil)
    (should-not (keymap-lookup advent-global-mode-map "C-c x"))
    (should-not (keymap-lookup advent-mode-map "C-c x"))))

(defun advent-test--bound-p (key)
  "Non-nil if KEY under the \"C-c a\" prefix runs a command here."
  (commandp (key-binding (kbd (concat "C-c a " key)))))

(ert-deftest advent-keymap-prefix-follows-the-modes ()
  "Global commands follow `global-advent-mode', local ones `advent-mode'."
  (advent-test--with-prefix "C-c a"
    (with-temp-buffer
      (let ((global-advent-mode t))
        (setq-local advent-mode nil)
        (should (advent-test--bound-p "l"))
        (should-not (advent-test--bound-p "i"))
        (setq-local advent-mode t)
        (should (advent-test--bound-p "l"))
        (should (advent-test--bound-p "i")))
      (let ((global-advent-mode nil))
        (setq-local advent-mode t)
        (should (advent-test--bound-p "l"))
        (should (advent-test--bound-p "i"))))))

(provide 'keymap-test)
;;; keymap-test.el ends here
