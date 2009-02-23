;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS
;;
(require 'emms-setup)
(emms-devel)
(setq emms-player-list
      '(emms-player-mpg321
	emms-player-ogg123))
(setq emms-info-asynchronosly t)
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")
(setq emms-player-mpg321-parameters '("-o" "alsa"))
(setq emms-source-list '((emms-directory-tree "~/muse/")))

(provide 'cm-emms)