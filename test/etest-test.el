(require 'ert)
(require 'rgr)

(defconst test-file-location (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest rgr-npm-has-package ()
  "Should return PACKAGE version if found, nil otherwise."
  (let ((default-directory (concat test-file-location "fixtures/npm/mocha/")))
    (should (equal (rgr--npm-has-package "mocha") "^9.1.3")))
  (let ((default-directory (concat test-file-location "fixtures/npm/jest/")))
    (should (equal (rgr--npm-has-package "jest") "1.0.0")))
  (let ((default-directory (concat test-file-location "fixtures/npm/empty/")))
    (should (equal (rgr--npm-has-package "jest") nil))))

(ert-deftest rgr-guess-project-type ()
  "Should return project type if guessed, nil otherwise."
  (let ((default-directory (concat test-file-location "fixtures/npm/mocha/")))
    (should (equal (rgr--guess-project-type) 'npm))))

(ert-deftest rgr-guess-test-runner ()
  (let ((default-directory (concat test-file-location "fixtures/npm/jest")))
    (should (equal (rgr--guess-project-runner) 'jest))))

(ert-deftest rgr-mocha-test-project ()
  (let* ((default-directory (concat test-file-location "fixtures/npm/mocha/"))
        (filename (concat default-directory "test/test.js")))
    (with-current-buffer (find-file filename)
      (should (equal (rgr--mocha-command-args)
                     `("node_modules/.bin/mocha"))))))

(ert-deftest rgr-mocha-test-file ()
  (let* ((default-directory (concat test-file-location "fixtures/npm/mocha/"))
        (filename (concat default-directory "test/test.js")))
    (with-current-buffer (find-file filename)
      (should (equal (rgr--mocha-command-args :file t)
                     `("node_modules/.bin/mocha" ,filename))))))

(ert-deftest rgr-mocha-test-dwim ()
  (let* ((default-directory (concat test-file-location "fixtures/npm/mocha/"))
         (filename (concat default-directory "test/test.js")))
    (with-current-buffer (find-file filename)
      (save-excursion
        (goto-char (point-min))
        (should (equal (rgr--mocha-command-args :dwim t)
                       `("node_modules/.bin/mocha" ,filename "--fgrep='one'"))))

      (save-excursion
        (goto-char (point-max))
        (should (equal (rgr--mocha-command-args :dwim t)
                       `("node_modules/.bin/mocha" ,filename))))

      (save-excursion
        (goto-char (point-min))
        (search-forward "two")
        (should (equal (rgr--mocha-command-args :dwim t)
                       `("node_modules/.bin/mocha" ,filename "--fgrep='two'")))))))
