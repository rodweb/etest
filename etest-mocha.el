(defcustom etest-mocha-program "node_modules/.bin/mocha"
  "Mocha's program path.")

(defcustom etest-mocha-reporter nil
  "Mocha's reporter."
  :type 'string)

(defcustom etest-mocha-identifiers '("describe" "it")
  "Mocha's test identifiers."
  :type '(repeat string))

(defun etest--mocha-check ()
  (let ((default-directory (projectile-project-root)))
    (etest--npm-has-package "mocha")))

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

(defun etest--mocha-command-args (&rest args)
  (etest--remove-nil
   (list etest-mocha-program
         (and etest-mocha-reporter
              (format "--reporter=%s" etest-mocha-reporter))
         (and (or (plist-get args :file) (plist-get args :dwim))
              (etest--current-filename))
         (and (plist-get args :dwim)
              (if-let ((name (etest--mocha-get-test-name)))
                  (format "--fgrep='%s'" name))))))

(provide 'etest-mocha)
