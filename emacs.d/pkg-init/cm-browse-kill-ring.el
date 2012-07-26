;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; browse kill ring
;;;

(autoload 'browse-kill-ring "browse-kill-ring" "")

(global-set-key (kbd "C-c n") 'browse-kill-ring)

(provide 'cm-browse-kill-ring)
