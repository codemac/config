;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown
;;
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook '(lambda ()
				 (flyspell-mode 1)
				 (auto-fill-mode 1)
				 ))
;; autoload
(add-to-list 'auto-mode-alist '("\\.mdwn$" . markdown-mode))

(provide 'cm-markdown)