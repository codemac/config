;; yay magit
(require 'magit-autoloads)
; add some signage.
(eval-after-load 'magit
  (setq magit-commit-signoff t))
(provide 'cm-magit)
