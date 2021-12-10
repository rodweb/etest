(require 'compile)
(require 'projectile)
(require 'tree-sitter)

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

(defun etest--mocha-test-project ()
  (let ((program "node_modules/.bin/mocha"))
    (list program "--reporter=dot")))

(defun etest--mocha-test-file ()
  (let ((program "node_modules/.bin/mocha")
        (filename (buffer-file-name)))
    (list program "--reporter=dot" filename)))

(defcustom etest-mocha-identifiers '("describe" "it")
  "Mocha's test identifiers."
  :type '(repeat string))

(defun etest--mocha-walk-up (node)
  (if-let ((identifier (and node (tsc-get-nth-named-child node 0))))
      (if (and (eq (tsc-node-type identifier) 'identifier)
               (member (tsc-node-text identifier) etest-mocha-identifiers))
          (tsc-get-nth-named-child (tsc-get-nth-named-child node 1) 0)
        (etest--walk-up (tsc-get-parent node)))))

(defun etest--mocha-get-test-name ()
  (if-let* ((node (tree-sitter-node-at-pos 'call_expression))
         (node (etest--mocha-walk-up node)))
    (substring (tsc-node-text node) 1 -1)))

(defun etest--mocha-test-dwim ()
  (let* ((program "node_modules/.bin/mocha")
         (filename (buffer-file-name))
         (name (etest--mocha-get-test-name)))
    (if (not name)
        (etest--mocha-test-file)
      (list program "--reporter=dot" filename "--fgrep" (concat "'" name "'")))))

(defun etest--run (action)
  (let* ((default-directory (projectile-project-root))
         (runner (etest--guess-project-runner))
         (command (etest--call-if-bound runner action)))
    (compile (mapconcat #'identity command " "))))

(defun etest-project ()
  (interactive)
  (etest--run "test-project"))

(defun etest-file ()
  (interactive)
  (etest--run "test-file"))

(defun etest-dwim ()
  (interactive)
  (etest--run "test-dwim"))

(provide 'etest)
