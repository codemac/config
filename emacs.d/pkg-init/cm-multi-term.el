;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multi term!
;;;
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/zsh") ;; or use zsh...

;; only needed if you use autopair
(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))


(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(provide 'cm-multi-term)
