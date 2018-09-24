;;; diffscuss-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "diffscuss-mode" "diffscuss-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from diffscuss-mode.el

(add-to-list 'auto-mode-alist '("\\.diffscuss\\'" . diffscuss-mode))

(autoload 'diffscuss-mode "diffscuss-mode" "\
Major mode for inter-diff code review.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "diffscuss-mode" '("diffscuss-" "backport-string-prefix-p")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; diffscuss-mode-autoloads.el ends here
