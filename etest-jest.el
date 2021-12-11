(defun etest--jest-check ()
  (let ((default-directory (projectile-project-root)))
    (etest--npm-has-package "jest")))

(provide 'etest-jest)
