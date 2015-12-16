;;; tup-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "tup-mode" "tup-mode.el" (22120 33625 217647
;;;;;;  500000))
;;; Generated autoloads from tup-mode.el

(autoload 'tup-mode "tup-mode" "\
Major mode for editing tupfiles for the Tup build system.

\\{tup-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.tup$" . tup-mode))

(add-to-list 'auto-mode-alist '("Tupfile" . tup-mode))

(add-to-list 'auto-mode-alist '("tup.config" . tup-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; tup-mode-autoloads.el ends here
