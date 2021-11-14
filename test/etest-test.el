(require 'ert)
(require 'etest)

(defconst test-file-location (file-name-directory (or load-file-name buffer-file-name)))

(ert-deftest etest-npm-has-package ()
  "Should return PACKAGE version if found, nil otherwise."
  (let ((default-directory (concat test-file-location "fixtures/npm/mocha/")))
    (should (equal (etest--npm-has-package "mocha") "^9.1.3")))
  (let ((default-directory (concat test-file-location "fixtures/npm/jest/")))
    (should (equal (etest--npm-has-package "jest") "1.0.0")))
  (let ((default-directory (concat test-file-location "fixtures/npm/empty/")))
    (should (equal (etest--npm-has-package "jest") nil))))

(ert-deftest etest-guess-project-type ()
  "Should return project type if guessed, nil otherwise."
  (let ((default-directory (concat test-file-location "fixtures/npm/mocha/")))
    (should (equal (etest--guess-project-type) 'npm))))

(ert-deftest etest-guess-test-runner ()
  (let ((default-directory (concat test-file-location "fixtures/npm/jest")))
    (should (equal (etest--guess-project-runner) 'jest))))

(ert-deftest etest-mocha-test-file ()
  (let* ((default-directory (concat test-file-location "fixtures/npm/mocha/"))
        (filename (concat default-directory "test/test.js")))
    (with-current-buffer (find-file filename)
      (should (equal (etest--mocha-test-file filename)
                     `("node_modules/.bin/mocha" "--reporter=dot" ,filename))))))
