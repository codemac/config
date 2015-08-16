;;; flycheck-tip-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "eclim-tip" "eclim-tip.el" (21967 58677 570203
;;;;;;  316000))
;;; Generated autoloads from eclim-tip.el

(autoload 'eclim-tip-cycle "eclim-tip" "\


\(fn &optional REVERSE)" t nil)

(autoload 'eclim-tip-cycle-reverse "eclim-tip" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil "error-tip" "error-tip.el" (21967 58677 563536
;;;;;;  646000))
;;; Generated autoloads from error-tip.el

(autoload 'error-tip-cycle-dwim "error-tip" "\
Showing error function.
This function switches proper error showing function by context.
 (whether flycheck or flymake) The REVERSE option jumps by inverse if
the value is non-nil.

\(fn &optional REVERSE)" t nil)

(autoload 'error-tip-cycle-dwim-reverse "error-tip" "\
Same as ‘error-tip-cycle-dwim’, but it jumps to inverse direction.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "flycheck-tip" "flycheck-tip.el" (21967 58677
;;;;;;  506869 946000))
;;; Generated autoloads from flycheck-tip.el

(autoload 'flycheck-tip-cycle "flycheck-tip" "\
Move to next error if it's exists.
If it wasn't exists then move to previous error.
Move to previous error if REVERSE is non-nil.

\(fn &optional REVERSE)" t nil)

(autoload 'flycheck-tip-cycle-reverse "flycheck-tip" "\
Do `flycheck-tip-cycle by reverse order.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "flymake-tip" "flymake-tip.el" (21967 58677
;;;;;;  533536 628000))
;;; Generated autoloads from flymake-tip.el

(autoload 'flymake-tip-cycle "flymake-tip" "\


\(fn REVERSE)" t nil)

(autoload 'flymake-tip-cycle-reverse "flymake-tip" "\


\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("flycheck-tip-pkg.el") (21967 58677 598990
;;;;;;  585000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-tip-autoloads.el ends here
