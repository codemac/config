;;;;;;;;;;;;;;;;;;;;;;;;; sj coding standards
(defun my-c-hook ()
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq tab-stop-list
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100))
  (setq fill-column 80)
  (setq-default c-basic-offset 4)
  (setq show-trailing-whitespace t)
  (setq c-tab-always-indent t)
  (linum-mode 1)
  (setq comment-multi-line t))

(add-hook 'cc-mode-hook 'my-c-hook)
(add-hook 'c-mode-hook 'my-c-hook)

(provide 'cm-c)
