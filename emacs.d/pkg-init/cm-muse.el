;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; muse
;;
(require 'muse-autoloads)
(add-hook 'muse-mode-hook '(lambda ()
			     (footnote-mode 1)
			     (flyspell-mode 1)
			     (auto-fill-mode 1)
			     ))
;; My wiki's!
(setq muse-project-alist
      '(("Personal Miki" ("~/miki/src" :default "index")
	 (:base "html" :path "~/miki/html"))))
;;

(provide 'cm-muse)