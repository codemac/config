;; time our .emacs loading
(defvar *emacs-load-start* (current-time))

;; (let* ((user-init-dir-default
;;     (file-name-as-directory (concat "~" init-file-user "/.emacs.d")))
;;        (user-init-dir
;;     (file-name-as-directory (or (getenv "EMACS_USER_DIRECTORY")
;;                     user-init-dir-default)))
;;        (user-init-file-1
;;     (expand-file-name "init" user-init-dir)))
;;   (setq user-emacs-directory user-init-dir)
;;   (with-eval-after-load "server"
;;     (setq server-name
;;       (let ((server--name (file-name-nondirectory
;;                    (directory-file-name user-emacs-directory))))
;;         (if (equal server--name ".emacs.d")
;;         "server"
;;           server--name))))
;;   (setq user-init-file t)
;;   (load user-init-file-1 t t)
;;   (when (eq user-init-file t)
;;     (setq user-emacs-directory user-init-dir-default)
;;     (load (expand-file-name "init" user-init-dir-default) t t)

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Use package management!
(require 'package)

(setq package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("gnu"          . 100)
        ("org"          . 90)
        ("melpa-stable" . 80)
        ("melpa"        . 70)))

(package-initialize)

; load up the main file
(require 'ob)
(require 'org)
(message (org-version))
(org-babel-load-file (expand-file-name "boot.org" dotfiles-dir))

;;; init.el ends here
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load-file custom-file)

(require 'cl)
(message "My .emacs loaded in %ds"
         (cl-destructuring-bind (hi lo ms ps) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
