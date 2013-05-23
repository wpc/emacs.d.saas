(defvar ruby-test-ruby-executable)
(set 'ruby-test-ruby-executable nil)

(defun ruby-test-run-use-project-local-jruby ()
  (interactive)
  (set 'ruby-test-ruby-executable (expand-file-name "script/jruby" (textmate-project-root))))

(defun ruby-test-run-use-global-ruby ()
  (interactive)
  (set 'ruby-test-ruby-executable nil))

(defun ruby-test-run-get-ruby-executable ()
  (or ruby-test-ruby-executable ruby-compilation-executable))

(defun find-ruby-testcase-name ()
  (interactive)
  (save-excursion
    (cond ((re-search-backward "^[ \\t]*def[ \\t]+\\(test[_a-z0-9]*\\)" nil t)
           (match-string 1))
          ((re-search-backward "^[ \\t]*test[ \\t]+['\"]\\(.+\\)['\"][ \\t]+do" nil t)
           (concat "test_" (replace-regexp-in-string " " "_" (match-string 1)))))))

(defvar last-ruby-test-command)
(defvar last-ruby-test-filename)

(defun ruby-test-run (&optional test-opts &optional loader-opts)
    (let* ((path (buffer-file-name))
         (filename (file-name-nondirectory path))
         (test-path (expand-file-name "test" (textmate-project-root)))
         (command (append (list (ruby-test-run-get-ruby-executable))
                          loader-opts
                          (list "-I" test-path path)
                          test-opts)))
      (set 'last-ruby-test-filename filename)
      (set 'last-ruby-test-command command)
      (pop-to-buffer (ruby-compilation-do filename command))))

(defun ruby-test-run-last ()
  (interactive)
  (pop-to-buffer (ruby-compilation-do last-ruby-test-filename last-ruby-test-command)))

(defun ruby-test-run-single ()
  (interactive)
  (ruby-test-run (list "-n" (find-ruby-testcase-name))))


(defun ruby-test-run-file (&optional test-opts)
  (interactive)
  (ruby-test-run (list)))


(provide 'ruby-test-run)
