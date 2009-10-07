;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gnus lock file
(defvar gnus-lock-filename)
(setq gnus-lock-filename "~/.machine-lock-gnus-my")
(put 'gnus 'disabled t)

(defun gf-touch (file)
  "Touches file"
  (save-excursion
    (unless (file-exists-p file)
      (find-file file)
      (write-file file)
      (kill-buffer (current-buffer)))))

(defun gnusu (&rest args)
  (interactive "P")
  (if (file-exists-p gnus-lock-filename)
  (error "Can't start gnus, Lock file exists %S" gnus-lock-filename)
(call-interactively 'gnus)))
;;;

(provide 'cm-gnus)