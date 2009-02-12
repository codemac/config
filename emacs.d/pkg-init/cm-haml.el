;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haml and such
;;
(autoload 'haml-mode "haml-mode" "" t)
(add-hook 'haml-mode-hook '(lambda ()
			     (setq indent-tabs-mode nil)
			     ))

(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(provide 'cm-haml)