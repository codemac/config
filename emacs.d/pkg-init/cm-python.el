;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python!

(add-hook 'python-mode-hook
	  `(lambda ()
	     (linum-mode 1)
	     (setq show-trailing-whitespace t)
	     ))

(provide 'cm-python)
