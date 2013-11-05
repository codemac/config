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

(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            "org-src" dotfiles-dir))) t)

; load up the main file
(require 'org-install)
(require 'ob)
(require 'org)
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-include-dirs (quote ("." "/Users/jmickey/code/Agda-2.3.2.1/std_lib/lib-0.7/src")))
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"] t)
 '(auto-image-file-mode t)
 '(browse-url-firefox-new-window-is-tab t)
 '(browse-url-firefox-program "firefox")
 '(custom-safe-themes (quote ("4e72cb2841e4801ba202a120c1cffdf88f5512536e557d03b3626d890b52f201" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(fci-rule-color "#383838")
 '(frame-background-mode (quote dark))
 '(haskell-hoogle-command "hoogle")
 '(indent-tabs-mode nil)
 '(notmuch-saved-searches (quote (("inbox" . "tag:inbox") ("unread" . "tag:unread") ("mars" . "folder:INBOX.mars -folder:INBOX.mars.volume +tag:unread"))))
 '(org-agenda-files (quote ("~/org/gtd.org" "~/org/fitness.org" "~/org/mars.org" "~/org/_notes/2012.org" "~/org/_notes/class2012pgm.org" "~/org/_notes/gifts.org" "~/org/_notes/nanowrimo2011.org" "~/org/_notes/notes.org" "~/org/_notes/oppression-of-tech.org" "~/org/_notes/steal.org" "~/org/_notes/webmac.org" "~/org/_notes/whoami.org")))
 '(safe-local-variable-values (quote ((yaml-indent-offset . 8) (after-save-hook archive-done-tasks))))
 '(yaml-indent-offset 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
