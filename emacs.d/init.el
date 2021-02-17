;;	Jeff Mickey's .emacs.d/init.el file
;;
;;	the awkward part is that I wrote this in vim :/
;;      UPDATE: this was written in emacs. BOOTSTRAPTIME

;; time our .emacs loading
(defvar *emacs-load-start* (current-time))

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Use package management!
(require 'package)

;; why the fuck aren't these https!
(setq package-archives
      '(("org"          . "https://orgmode.org/elpa/")
	("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	`((org              . "org")
	  (org-ql           . "melpa")
	  (org-sidebar      . "melpa")
	  (org-super-agenda . "melpa"))))

(setq package-archive-priorities
      '(("gnu"          . 100)
	("org"          . 90)
	("melpa"        . 80)))

(package-initialize)

; load up the main file
(require 'ob)
(require 'org)
(message (org-version))
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load-file custom-file)

;;
;; need a better way to do machine-dependent settings in emacs. Some
;; need to happen before anything, some need to happen after
;; anything... it's really quite silly.
;;
;; In this case it's that I want my work settings to take precedence
;; over anything that happens in my boot.org file. Though this now
;; means that any libraries that are loaded based on settings from
;; above are misconfigured. This is rather unacceptable.
(setq work-emacs-file (expand-file-name "~/.emacs-work.el"))
(load-file work-emacs-file)

(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (cl-first *emacs-load-start*) (cl-second *emacs-load-start*)))))
