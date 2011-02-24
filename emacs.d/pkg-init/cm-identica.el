;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; woo identica
(autoload 'identica-mode "identica-mode" "" t)
(setq identica-username cm-identica-username
      identica-password cm-identica-password)

(global-set-key "\C-cip" 'identica-update-status-interactive)
(global-set-key "\C-cid" 'identica-direct-message-interactive)

(provide 'cm-identica)
