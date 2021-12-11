(require 'compile)
(require 'projectile)
(require 'tree-sitter)
(require 'etest-mocha)
(require 'etest-jest)

(defun etest--npm-has-package (package)
  "Returns PACKAGE version if found, nil otherwise."
  (let* ((json-hash (with-temp-buffer
                      (insert-file-contents "package.json")
                      (json-parse-buffer)))
         (dependencies (gethash "dependencies" json-hash))
         (devDependencies (gethash "devDependencies" json-hash)))
    (or (and dependencies (gethash package dependencies))
        (and devDependencies (gethash package devDependencies)))))

(defun etest--guess-project-type ()
  (projectile-project-type))

(defvar etest--test-runners
  '((npm . (mocha jest)))
  "Test runners.")

(defun etest--guess-project-runner ()
  (let* ((project-type (projectile-project-type))
         (runners (alist-get project-type etest--test-runners)))
    (seq-find (lambda (runner)
                (etest--call-if-bound runner "check"))
              runners)))

(defun etest--call-if-bound (runner fn &optional args)
  (let ((fn (intern (concat "etest--" (symbol-name runner) "-" fn))))
    (if (fboundp fn)
        (apply fn args)
      (error "%s not supported for runner %s" fn runner))))

(defun etest--remove-nil (items)
  (seq-remove #'not items))

(defun etest--current-filename ()
  (buffer-file-name))

(defun etest--run (&rest args)
  (let* ((default-directory (projectile-project-root))
         (runner (etest--guess-project-runner))
         (command (etest--call-if-bound runner "command-args" args)))
    (compile (mapconcat #'identity command " "))))

(defun etest-project ()
  (interactive)
  (etest--run))

(defun etest-file ()
  (interactive)
  (etest--run :file t))

(defun etest-dwim ()
  (interactive)
  (etest--run :dwim t))

(provide 'etest)
