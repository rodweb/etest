(require 'compile)
(require 'projectile)

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

(defun etest--mocha-check ()
  (let ((default-directory (projectile-project-root)))
    (etest--npm-has-package "mocha")))

(defun etest--jest-check ()
  (let ((default-directory (projectile-project-root)))
    (etest--npm-has-package "jest")))

(defvar etest--test-runners
  '((npm . (mocha jest)))
  "Test runners.")

(defun etest--guess-project-runner ()
  (let* ((project-type (projectile-project-type))
         (runners (alist-get project-type etest--test-runners)))
    (seq-find (lambda (runner)
                (etest--call-if-bound runner "check"))
              runners)))

(defun etest--call-if-bound (runner fn &rest args)
  (let ((fn (intern (concat "etest--" (symbol-name runner) "-" fn))))
    (if (fboundp fn)
        (apply fn args)
      (error "%s not supported for runner %s" fn runner))))

(defun etest--mocha-test-file (filename)
  (let ((program "node_modules/.bin/mocha"))
    (list program "--reporter=dot" filename)))

(defun etest-file ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (runner (etest--guess-project-runner))
         (filename (buffer-file-name))
         (command (etest--call-if-bound runner "test-file" filename)))
    (compile (mapconcat #'identity command " "))))

(provide 'etest)
