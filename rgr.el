;;; rgr.el --- An Emacs minor mode for running tests.

;; Author: Rodrigo Campos
;; Version: 0.1
;; Package-Requires: ((emacs "28.0") (projectile "2.6"))
;; Keywords: testing, tdd
;; URL: https://github.com/rodweb/rgr.el

;;; Commentary:
;;; Code:
(require 'compile)
(require 'projectile)
(require 'tree-sitter)
(require 'rgr-mocha)
(require 'rgr-jest)
(require 'rgr-go)

(defun rgr--npm-has-package (package)
  "Return PACKAGE version if found, nil otherwise."
  (let* ((json-hash (with-temp-buffer
                      (insert-file-contents "package.json")
                      (json-parse-buffer)))
         (dependencies (gethash "dependencies" json-hash))
         (devDependencies (gethash "devDependencies" json-hash)))
    (or (and dependencies (gethash package dependencies))
        (and devDependencies (gethash package devDependencies)))))

(defun rgr--guess-project-type ()
  "Guess project type."
  (projectile-project-type))

(defvar rgr--test-runners
  '((npm . (mocha jest))
    (go . (go)))
  "Supported test runners by project types.")

(defun rgr--guess-project-runner ()
  "Guess project runner based on project type."
  (let* ((project-type (rgr--guess-project-type))
         (runners (alist-get project-type rgr--test-runners)))
    (seq-find (lambda (runner)
                (rgr--call-if-bound runner "check"))
              runners)))

(defun rgr--call-if-bound (runner fn &optional args)
  "If there is a function in the form of rgr-- RUNNER - FN, call it with ARGS."
  (let ((fn (intern (concat "rgr--" (symbol-name runner) "-" fn))))
    (if (fboundp fn)
        (apply fn args)
      (error "%s not supported for runner %s" fn runner))))

(defun rgr--remove-nil (items)
  "Return a new sequence with nil elements removed from ITEMS."
  (seq-remove #'not items))

(defun rgr--current-filename ()
  "Return the current filename."
  (buffer-file-name))

(defvar rgr-last-command
  "Last command ran." nil)

(defun rgr--run (&rest args)
  "Run test runner's command-args function with the given ARGS."
  (let ((default-directory (funcall rgr-project-root-function))
        (compile-command nil))
    (if (plist-get args :repeat)
        (compile rgr-last-command)
      (let* ((runner (rgr--guess-project-runner))
             (command (rgr--call-if-bound runner "command-args" args))
             (command-string (mapconcat #'identity command " ")))
        (setq rgr-last-command command-string)
        (compile command-string)))))

(defun rgr-project ()
  "Run project tests."
  (interactive)
  (rgr--run))

(defun rgr-file ()
  "Run file tests."
  (interactive)
  (rgr--run :file t))

(defun rgr-dwim ()
  "Run test at point."
  (interactive)
  (rgr--run :dwim t))

(defun rgr-last ()
  "Repeat last test run."
  (interactive)
  (rgr--run :repeat t))

(defcustom rgr-project-root-function #'projectile-project-root
  "Function to query for the project root."
  :group 'rgr-mode)

(defcustom rgr-command-prefix "C-c t"
  "Command prefix for `rgr-mode'."
  :group 'rgr-mode)

(defvar rgr-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'rgr-project)
    (define-key map (kbd "f") #'rgr-file)
    (define-key map (kbd "l") #'rgr-last)
    (define-key map (kbd "t") #'rgr-dwim)
    map)
  "Keymap for `rgr-mode' commands.")
(fset 'rgr-command-map rgr-command-map)

(defvar rgr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd rgr-command-prefix) rgr-command-map)
    map)
  "Keymap for `rgr-mode'.")

(define-minor-mode rgr-mode
  "An Emacs minor-mode for running tests."
  :lighter "rgr")

(provide 'rgr)
;;; rgr.el ends here
