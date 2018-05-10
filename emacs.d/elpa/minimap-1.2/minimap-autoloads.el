;;; minimap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "minimap" "minimap.el" (0 0 0 0))
;;; Generated autoloads from minimap.el

(defvar minimap-mode nil "\
Non-nil if Minimap mode is enabled.
See the `minimap-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `minimap-mode'.")

(custom-autoload 'minimap-mode "minimap" nil)

(autoload 'minimap-mode "minimap" "\
Toggle minimap mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "minimap" '("minimap-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; minimap-autoloads.el ends here
