(defun rails-toolbelt-server ()
  (interactive)
  (let* ((path (buffer-file-name))
         (filename (file-name-nondirectory path))
         (command (append (list ruby-compilation-executable)
                          (list "-S" "rails" "server"))))
      (pop-to-buffer (ruby-compilation-do filename command))))

(provide 'rails-toolbelt)
