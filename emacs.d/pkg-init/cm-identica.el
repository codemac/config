;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; woo identica
(require 'identica-mode)
(setq identica-username cm-identica-username
      identica-password cm-identica-password)

(global-set-key "\C-cip" 'identica-update-status-interactive)
(global-set-key "\C-cid" 'identica-direct-message-interactive)

(provide 'cm-identica)