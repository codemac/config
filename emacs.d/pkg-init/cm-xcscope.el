(require 'xcscope)
(setq cscope-do-not-update-database t)
(defun xcscope-minor-mode ()
  (interactive)
  (cscope:hook)
)

(add-hook 'python-mode-hook (function cscope:hook))

(provide 'cm-xcscope)