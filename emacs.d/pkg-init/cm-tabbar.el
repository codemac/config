;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tabbar
;;; Get some smarter tabs.
(require 'tabbar)
(tabbar-mode)

(global-set-key (kbd "<C-tab>") 'tabbar-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-forward-group)

(provide 'cm-tabbar)
