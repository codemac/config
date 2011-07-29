;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let's restructure some text
(add-to-list 'auto-mode-alist '("\\.rst$" . rst-mode))

(add-hook 'text-mode-hook 'rst-text-mode-bindings)
(add-hook 'rst-adjust-hook 'rst-toc-insert-update)