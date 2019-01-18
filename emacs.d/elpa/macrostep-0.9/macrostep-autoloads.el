;;; macrostep-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "macrostep" "../../../../.emacs.d/elpa/macrostep-0.9/macrostep.el"
;;;;;;  "c4c7cddea73734e122ad4f2d61e442e2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/macrostep-0.9/macrostep.el

(autoload 'macrostep-mode "macrostep" "\
Minor mode for inline expansion of macros in Emacs Lisp source buffers.

\\<macrostep-keymap>Progressively expand macro forms with \\[macrostep-expand], collapse them with \\[macrostep-collapse],
and move back and forth with \\[macrostep-next-macro] and \\[macrostep-prev-macro].
Use \\[macrostep-collapse-all] or collapse all visible expansions to
quit and return to normal editing.

\\{macrostep-keymap}

\(fn &optional ARG)" t nil)

(autoload 'macrostep-expand "macrostep" "\
Expand the macro form following point by one step.

Enters `macrostep-mode' if it is not already active, making the
buffer temporarily read-only. If macrostep-mode is active and the
form following point is not a macro form, search forward in the
buffer and expand the next macro form found, if any.

With a prefix argument, the expansion is displayed in a separate
buffer instead of inline in the current buffer.  Setting
`macrostep-expand-in-separate-buffer' to non-nil swaps these two
behaviors.

\(fn &optional TOGGLE-SEPARATE-BUFFER)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "macrostep" "../../../../.emacs.d/elpa/macrostep-0.9/macrostep.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/macrostep-0.9/macrostep.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macrostep" '("macrostep-")))

;;;***

;;;***

;;;### (autoloads nil "macrostep-c" "../../../../.emacs.d/elpa/macrostep-0.9/macrostep-c.el"
;;;;;;  "69a7229b1a64aa4d3e8996b64c98d114")
;;; Generated autoloads from ../../../../.emacs.d/elpa/macrostep-0.9/macrostep-c.el

(autoload 'macrostep-c-mode-hook "macrostep-c" "\


\(fn)" nil nil)

(add-hook 'c-mode-hook #'macrostep-c-mode-hook)

;;;### (autoloads "actual autoloads are elsewhere" "macrostep-c"
;;;;;;  "../../../../.emacs.d/elpa/macrostep-0.9/macrostep-c.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/macrostep-0.9/macrostep-c.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "macrostep-c" '("macrostep-c-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/macrostep-0.9/macrostep-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/macrostep-0.9/macrostep-c.el"
;;;;;;  "../../../../.emacs.d/elpa/macrostep-0.9/macrostep-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/macrostep-0.9/macrostep.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; macrostep-autoloads.el ends here
