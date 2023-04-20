;;; rmsbolt-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rmsbolt" "rmsbolt.el" (0 0 0 0))
;;; Generated autoloads from rmsbolt.el

(autoload 'rmsbolt-starter "rmsbolt" "\
Setup new file based on the sample STARTER-FILE-NAME.

\(fn LANG-NAME)" t nil)

(autoload 'rmsbolt-mode "rmsbolt" "\
Toggle rmsbolt-mode.

This is a minor mode.  If called interactively, toggle the
`Rmsbolt mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `rmsbolt-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

This mode is enabled in both src and assembly output buffers.

\(fn &optional ARG)" t nil)

(autoload 'rmsbolt "rmsbolt" "\
Start a rmsbolt compilation and enable `rmsbolt-mode' for code region
highlighting and automatic recompilation." t nil)

(register-definition-prefixes "rmsbolt" '("rmsbolt-"))

;;;***

;;;### (autoloads nil "rmsbolt-java" "rmsbolt-java.el" (0 0 0 0))
;;; Generated autoloads from rmsbolt-java.el

(register-definition-prefixes "rmsbolt-java" '("rmsbolt-java-"))

;;;***

;;;### (autoloads nil "rmsbolt-split" "rmsbolt-split.el" (0 0 0 0))
;;; Generated autoloads from rmsbolt-split.el

(register-definition-prefixes "rmsbolt-split" '("rmsbolt-split-"))

;;;***

;;;### (autoloads nil nil ("rmsbolt-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rmsbolt-autoloads.el ends here
