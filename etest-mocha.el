(defcustom rgr-mocha-program "node_modules/.bin/mocha"
  "Mocha's program path.")

(defcustom rgr-mocha-reporter nil
  "Mocha's reporter."
  :type 'string)

(defcustom rgr-mocha-identifiers '("describe" "it")
  "Mocha's test identifiers."
  :type '(repeat string))

(defun rgr--mocha-check ()
  (let ((default-directory (projectile-project-root)))
    (rgr--npm-has-package "mocha")))

(defun rgr--mocha-walk-up (node)
  (if-let ((identifier (and node (tsc-get-nth-named-child node 0))))
      (if (and (eq (tsc-node-type identifier) 'identifier)
               (member (tsc-node-text identifier) rgr-mocha-identifiers))
          (tsc-get-nth-named-child (tsc-get-nth-named-child node 1) 0)
        (rgr--mocha-walk-up (tsc-get-parent node)))))

(defun rgr--mocha-get-test-name ()
  (if-let* ((node (tree-sitter-node-at-pos 'call_expression))
         (node (rgr--mocha-walk-up node)))
    (substring (tsc-node-text node) 1 -1)))

(defun rgr--mocha-command-args (&rest args)
  (rgr--remove-nil
   (list rgr-mocha-program
         (and rgr-mocha-reporter
              (format "--reporter=%s" rgr-mocha-reporter))
         (and (or (plist-get args :file) (plist-get args :dwim))
              (rgr--current-filename))
         (and (plist-get args :dwim)
              (if-let ((name (rgr--mocha-get-test-name)))
                  (format "--fgrep='%s'" name))))))

(provide 'rgr-mocha)
