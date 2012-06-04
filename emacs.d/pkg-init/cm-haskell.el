;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haskell
;;
(autoload 'haskell-mode "haskell-mode.el" "" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook '(lambda ()
                                (linum-mode 1)))

(provide 'cm-haskell)
