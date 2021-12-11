(require 'compile)
(require 'projectile)
(require 'tree-sitter)
(require 'rgr-mocha)
(require 'rgr-jest)

(defun rgr--npm-has-package (package)
  "Returns PACKAGE version if found, nil otherwise."
  (let* ((json-hash (with-temp-buffer
                      (insert-file-contents "package.json")
                      (json-parse-buffer)))
         (dependencies (gethash "dependencies" json-hash))
         (devDependencies (gethash "devDependencies" json-hash)))
    (or (and dependencies (gethash package dependencies))
        (and devDependencies (gethash package devDependencies)))))

(defun rgr--guess-project-type ()
  (projectile-project-type))

(defvar rgr--test-runners
  '((npm . (mocha jest)))
  "Test runners.")

(defun rgr--guess-project-runner ()
  (let* ((project-type (projectile-project-type))
         (runners (alist-get project-type rgr--test-runners)))
    (seq-find (lambda (runner)
                (rgr--call-if-bound runner "check"))
              runners)))

(defun rgr--call-if-bound (runner fn &optional args)
  (let ((fn (intern (concat "rgr--" (symbol-name runner) "-" fn))))
    (if (fboundp fn)
        (apply fn args)
      (error "%s not supported for runner %s" fn runner))))

(defun rgr--remove-nil (items)
  (seq-remove #'not items))

(defun rgr--current-filename ()
  (buffer-file-name))

(defun rgr--run (&rest args)
  (let* ((default-directory (projectile-project-root))
         (runner (rgr--guess-project-runner))
         (command (rgr--call-if-bound runner "command-args" args)))
    (compile (mapconcat #'identity command " "))))

(defun rgr-project ()
  (interactive)
  (rgr--run))

(defun rgr-file ()
  (interactive)
  (rgr--run :file t))

(defun rgr-dwim ()
  (interactive)
  (rgr--run :dwim t))

(provide 'rgr)
