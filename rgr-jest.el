(defcustom rgr-jest-program "node_modules/.bin/jest"
  "Jest's program path.")

(defcustom rgr-jest-env nil
  "Jest's environment variables."
  :type 'string)

(defcustom rgr-jest-reporter nil
  "Jest's reporter."
  :type 'string)

(defcustom rgr-jest-identifiers '("describe" "it")
  "Jest's test identifiers."
  :type '(repeat string))

(defun rgr--jest-check ()
  (let ((default-directory (projectile-project-root)))
    (rgr--npm-has-package "jest")))

(defun rgr--jest-walk-up (node)
  (if-let ((identifier (and node (tsc-get-nth-named-child node 0))))
      (if (and (eq (tsc-node-type identifier) 'identifier)
               (member (tsc-node-text identifier) rgr-jest-identifiers))
          (tsc-get-nth-named-child (tsc-get-nth-named-child node 1) 0)
        (rgr--jest-walk-up (tsc-get-parent node)))))

(defun rgr--jest-get-test-name ()
  (if-let* ((node (tree-sitter-node-at-pos 'call_expression))
         (node (rgr--jest-walk-up node)))
    (substring (tsc-node-text node) 1 -1)))

(defun rgr--jest-command-args (&rest args)
  (rgr--remove-nil
   (list rgr-jest-env
         rgr-jest-program
         (and rgr-jest-reporter
              (format "--reporter=%s" rgr-jest-reporter))
         (and (or (plist-get args :file) (plist-get args :dwim))
              (rgr--current-filename))
         (and (plist-get args :dwim)
              (if-let ((name (rgr--jest-get-test-name)))
                  (format "-t='%s'" name))))))

(provide 'rgr-jest)
