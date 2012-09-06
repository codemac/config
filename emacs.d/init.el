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
;; Load up Org Mode and Babel
(require 'org-install)

;; load up the main file
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here


