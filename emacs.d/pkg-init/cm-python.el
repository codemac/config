;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python!

(add-hook 'python-mode-hook
	  `(lambda ()
	     (linum-mode 1)))

(provide 'cm-python)