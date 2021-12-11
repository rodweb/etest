(defun rgr--jest-check ()
  (let ((default-directory (projectile-project-root)))
    (rgr--npm-has-package "jest")))

(provide 'rgr-jest)
