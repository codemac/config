;;	Jeff Mickey's .emacs.d/init.el file
;;
;;	the awkward part is that I wrote this in vim :/
;;      UPDATE: this was written in emacs. BOOTSTRAPTIME

;; time our .emacs loading
(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org-src" dotfiles-dir)))
;; load up the main file
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "firefox")
 '(frame-background-mode (quote dark))
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/org/fitness.org" "~/org/from-mobile.org" "~/org/gtd.org" "~/org/mars.org" "~/org/_notes/2012.org" "~/org/_notes/class2012pgm.org" "~/org/_notes/gifts.org" "~/org/_notes/nanowrimo2011.org" "~/org/_notes/notes.org" "~/org/_notes/oppression-of-tech.org" "~/org/_notes/steal.org" "~/org/_notes/webmac.org" "~/org/_notes/whoami.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
