;;; helm-gtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-gtags-mode helm-gtags-update-tags helm-gtags-clear-all-stacks
;;;;;;  helm-gtags-clear-stack helm-gtags-show-stack helm-gtags-pop-stack
;;;;;;  helm-gtags-parse-file helm-gtags-find-tag-from-here helm-gtags-find-files
;;;;;;  helm-gtags-find-symbol helm-gtags-find-rtag helm-gtags-find-tag
;;;;;;  helm-gtags-select-path helm-gtags-select) "helm-gtags" "helm-gtags.el"
;;;;;;  (21111 6393 0 0))
;;; Generated autoloads from helm-gtags.el

(autoload 'helm-gtags-select "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-select-path "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-find-tag "helm-gtags" "\
Jump to definition

\(fn)" t nil)

(autoload 'helm-gtags-find-rtag "helm-gtags" "\
Jump to referenced point

\(fn)" t nil)

(autoload 'helm-gtags-find-symbol "helm-gtags" "\
Jump to the symbol location

\(fn)" t nil)

(autoload 'helm-gtags-find-files "helm-gtags" "\
Find file with gnu global

\(fn)" t nil)

(autoload 'helm-gtags-find-tag-from-here "helm-gtags" "\
Find from here with gnu global

\(fn)" t nil)

(autoload 'helm-gtags-parse-file "helm-gtags" "\
Find file with gnu global

\(fn)" t nil)

(autoload 'helm-gtags-pop-stack "helm-gtags" "\
Jump to previous point on the stack

\(fn)" t nil)

(autoload 'helm-gtags-show-stack "helm-gtags" "\
Show context stack

\(fn)" t nil)

(autoload 'helm-gtags-clear-stack "helm-gtags" "\
Clear jumped point stack

\(fn)" t nil)

(autoload 'helm-gtags-clear-all-stacks "helm-gtags" "\
Clear all jumped point stacks

\(fn)" t nil)

(autoload 'helm-gtags-update-tags "helm-gtags" "\
Update TAG file. Update All files with `C-u' prefix

\(fn)" t nil)

(autoload 'helm-gtags-mode "helm-gtags" "\
Toggle Helm-Gtags mode on or off.
With a prefix argument ARG, enable Helm-Gtags mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{helm-gtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-gtags-pkg.el") (21111 6393 881703
;;;;;;  0))

;;;***

(provide 'helm-gtags-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-gtags-autoloads.el ends here
