;;	Jeff Mickey's .emacs.d/init.el file
;;
;;	the awkward part is that I wrote this in vim :/
;;      UPDATE: this was written in emacs. BOOTSTRAPTIME

;; time our .emacs loading

(defvar *emacs-load-start* (current-time))

(setq cm/old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 1000 1000 1000))
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Use package management!
(require 'package)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
	("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	`((org              . "gnu")
	  (org-ql           . "melpa")
	  (org-sidebar      . "melpa")
	  (org-super-agenda . "melpa"))))

(setq package-archive-priorities
      '(("gnu"          . 100)
	("melpa"        . 90)
	("nongnu"       . 80)))

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
(when (file-exists-p work-emacs-file)
  (load-file work-emacs-file))

;; set back!
(setq gc-cons-threshold cm/old-gc-cons-threshold)

(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (cl-first *emacs-load-start*) (cl-second *emacs-load-start*)))))
