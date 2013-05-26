;;; geiser-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "geiser" "geiser.el" (20868 12969 0 0))
;;; Generated autoloads from geiser.el

(autoload 'geiser-version "geiser-version" "\
Echo Geiser's version." t)

(autoload 'geiser-unload "geiser-reload" "\
Unload all Geiser code." t)

(autoload 'geiser-reload "geiser-reload" "\
Reload Geiser code." t)

(autoload 'geiser "geiser-repl" "\
Start a Geiser REPL, or switch to a running one." t)

(autoload 'run-geiser "geiser-repl" "\
Start a Geiser REPL." t)

(autoload 'geiser-connect "geiser-repl" "\
Start a Geiser REPL connected to a remote server." t)

(autoload 'switch-to-geiser "geiser-repl" "\
Switch to a running one Geiser REPL." t)

(autoload 'run-guile "geiser-guile" "\
Start a Geiser Guile REPL." t)

(autoload 'switch-to-guile "geiser-guile" "\
Start a Geiser Guile REPL, or switch to a running one." t)

(autoload 'connect-to-guile "geiser-guile" "\
Connect to a remote Geiser Guile REPL." t)

(autoload 'run-racket "geiser-racket" "\
Start a Geiser Racket REPL." t)

(autoload 'run-gracket "geiser-racket" "\
Start a Geiser GRacket REPL." t)

(autoload 'switch-to-racket "geiser-racket" "\
Start a Geiser Racket REPL, or switch to a running one." t)

(autoload 'connect-to-racket "geiser-racket" "\
Connect to a remote Geiser Racket REPL." t)

(autoload 'geiser-mode "geiser-mode" "\
Minor mode adding Geiser REPL interaction to Scheme buffers." t)

(autoload 'turn-on-geiser-mode "geiser-mode" "\
Enable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'turn-off-geiser-mode "geiser-mode" "\
Disable Geiser's mode (useful in Scheme buffers)." t)

(autoload 'geiser-mode--maybe-activate "geiser-mode")

(mapc (lambda (group) (custom-add-load group (symbol-name group)) (custom-add-load 'geiser (symbol-name group))) '(geiser geiser-repl geiser-autodoc geiser-doc geiser-debug geiser-faces geiser-mode geiser-guile geiser-image geiser-racket geiser-implementation geiser-xref))

(add-hook 'scheme-mode-hook 'geiser-mode--maybe-activate)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))

;;;***

;;;### (autoloads nil nil ("geiser-autodoc.el" "geiser-base.el" "geiser-company.el"
;;;;;;  "geiser-compile.el" "geiser-completion.el" "geiser-connection.el"
;;;;;;  "geiser-custom.el" "geiser-debug.el" "geiser-doc.el" "geiser-edit.el"
;;;;;;  "geiser-eval.el" "geiser-guile.el" "geiser-image.el" "geiser-impl.el"
;;;;;;  "geiser-log.el" "geiser-menu.el" "geiser-mode.el" "geiser-pkg.el"
;;;;;;  "geiser-popup.el" "geiser-racket.el" "geiser-reload.el" "geiser-repl.el"
;;;;;;  "geiser-syntax.el" "geiser-table.el" "geiser-version.el"
;;;;;;  "geiser-xref.el") (20868 12969 766488 0))

;;;***

(provide 'geiser-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; geiser-autoloads.el ends here
