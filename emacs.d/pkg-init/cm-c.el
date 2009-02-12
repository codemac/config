;;;;;;;;;;;;;;;;;;;;;;;;; sj coding standards
(add-hook 'c-mode-hook
	  '(lambda ()
	     (setq tab-width 4)
	     (setq indent-tabs-mode 1)
	     (setq tab-stop-list 
		   '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
	     (setq fill-column 80)
	     (setq c-basic-offset 8)
	     (setq c-tab-always-indent t)
	     (setq comment-multi-line t)))

(provide 'cm-c)