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

(defun etest--call-if-bound (runner fn)
  (let ((fn (intern (concat "etest--" (symbol-name runner) "-" fn))))
    (if (fboundp fn)
        (funcall fn)
      (error "%s not supported for runner %s" runner fn))))

(provide 'etest)
