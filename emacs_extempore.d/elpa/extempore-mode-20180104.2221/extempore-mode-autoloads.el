;;; extempore-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "extempore-mode" "extempore-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from extempore-mode.el

(autoload 'extempore-mode "extempore-mode" "\
Major mode for editing Extempore code. This mode has been
adapted from `scheme-mode'. Entry to this mode calls the value of
\\[extempore-mode-hook].

To switch to an inferior Extempore process (or start one if none
present) use \\[switch-to-extempore], which is bound to C-c C-z
by default.

To send the current definition to a running Extempore process, use
\\[extempore-send-definition].

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

(autoload 'extempore-repl "extempore-mode" "\


\(fn HOST PORT)" t nil)

(autoload 'extempore-run "extempore-mode" "\
Run an inferior Extempore process, input and output via buffer `*extempore*'.
If there is a process already running in `*extempore*', switch to that buffer.

\(Type \\[describe-mode] in the process buffer for a list of commands.)

\(fn PROGRAM-ARGS RUN-DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "extempore-mode" '("extempore-" "next-sexp-as-string" "note-to-midi" "hex-to-decimal-at-point" "xpb1" "switch-to-extempore" "inferior-extempore-mode" "chomp" "would-be-symbol" "user-extempore-directory")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; extempore-mode-autoloads.el ends here
