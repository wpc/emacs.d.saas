(defun find-ruby-testcase-name ()
  (save-excursion
    (if (re-search-backward "^[ \\t]*def[ \\t]+\\(test[_a-z0-9]*\\)" nil t)
	(match-string 1))))

(defvar last-ruby-test-command)
(defvar last-ruby-test-filename)

(defun ruby-test-run (&optional test-opts &optional loader-opts)
    (let* ((path (buffer-file-name))
         (filename (file-name-nondirectory path))
         (test-path (expand-file-name "test" (textmate-project-root)))
         (command (append (list ruby-compilation-executable)
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
