;;; helm-gtags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "helm-gtags" "helm-gtags.el" (21967 58664 333528
;;;;;;  887000))
;;; Generated autoloads from helm-gtags.el

(autoload 'helm-gtags-clear-all-cache "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-clear-cache "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-next-history "helm-gtags" "\
Jump to next position on context stack

\(fn)" t nil)

(autoload 'helm-gtags-previous-history "helm-gtags" "\
Jump to previous position on context stack

\(fn)" t nil)

(autoload 'helm-gtags-select "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-select-path "helm-gtags" "\


\(fn)" t nil)

(autoload 'helm-gtags-tags-in-this-function "helm-gtags" "\
Show tagnames which are referenced in this function and jump to it.

\(fn)" t nil)

(autoload 'helm-gtags-create-tags "helm-gtags" "\


\(fn DIR LABEL)" t nil)

(autoload 'helm-gtags-find-tag "helm-gtags" "\
Jump to definition

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-tag-other-window "helm-gtags" "\
Jump to definition in other window.

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-rtag "helm-gtags" "\
Jump to referenced point

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-symbol "helm-gtags" "\
Jump to the symbol location

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-pattern "helm-gtags" "\
Grep and jump by gtags tag files.

\(fn PATTERN)" t nil)

(autoload 'helm-gtags-find-files "helm-gtags" "\
Find file from tagged with gnu global.

\(fn FILE)" t nil)

(autoload 'helm-gtags-find-tag-from-here "helm-gtags" "\
Jump point by current point information.
Jump to definition point if cursor is on its reference.
Jump to reference point if curosr is on its definition

\(fn)" t nil)

(autoload 'helm-gtags-dwim "helm-gtags" "\
Find by context. Here is
- on include statement then jump to included file
- on symbol definition then jump to its references
- on reference point then jump to its definition.

\(fn)" t nil)

(autoload 'helm-gtags-parse-file "helm-gtags" "\
Parse current file with gnu global. This is similar to `imenu'.
You can jump definitions of functions, symbols in this file.

\(fn)" t nil)

(autoload 'helm-gtags-pop-stack "helm-gtags" "\
Jump to previous point on the context stack and pop it from stack.

\(fn)" t nil)

(autoload 'helm-gtags-show-stack "helm-gtags" "\
Show current context stack.

\(fn)" t nil)

(autoload 'helm-gtags-clear-stack "helm-gtags" "\
Clear current context stack.

\(fn)" t nil)

(autoload 'helm-gtags-clear-all-stacks "helm-gtags" "\
Clear all context stacks.

\(fn)" t nil)

(autoload 'helm-gtags-update-tags "helm-gtags" "\
Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'

\(fn)" t nil)

(autoload 'helm-gtags-resume "helm-gtags" "\
Resurrect previously invoked `helm-gtags` command.

\(fn)" t nil)

(autoload 'helm-gtags-mode "helm-gtags" "\
Toggle Helm-Gtags mode on or off.
With a prefix argument ARG, enable Helm-Gtags mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{helm-gtags-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; helm-gtags-autoloads.el ends here
