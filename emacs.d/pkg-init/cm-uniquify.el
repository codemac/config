;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; uniquify
;; let's make these projects not blow.
(require 'uniquify)

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

(provide 'cm-uniquify)
