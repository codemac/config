;;	Jeff Mickey's .emacs.d/init.el file
;;
;;	the awkward part is that I wrote this in vim :/
;;      UPDATE: this was written in emacs. BOOTSTRAPTIME

;; time our .emacs loading
(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Use package management!
(require 'package)

(setq package-archives
      (append '(("org"       . "http://orgmode.org/elpa/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")
                ("marmalade" . "http://marmalade-repo.org/packages/"))
              package-archives))

(package-initialize)

; load up the main file
(require 'ob)
(require 'org)
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load-file custom-file)

(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
