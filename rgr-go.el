(defun rgr--go-check ()
  (eq major-mode 'go-mode))

(defun rgr--go-command-args (&rest args)
  (list "go" "test" (rgr--current-filename) (string-replace "_test" "" (rgr--current-filename))))

(provide 'rgr-go)
