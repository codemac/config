;;; clipetty-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "clipetty" "clipetty.el" (0 0 0 0))
;;; Generated autoloads from clipetty.el

(autoload 'clipetty-mode "clipetty" "\
Minor mode to send every kill from a TTY frame to the system clipboard.

\(fn &optional ARG)" t nil)

(defvar global-clipetty-mode nil "\
Non-nil if Global Clipetty mode is enabled.
See the `global-clipetty-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-clipetty-mode'.")

(custom-autoload 'global-clipetty-mode "clipetty" nil)

(autoload 'global-clipetty-mode "clipetty" "\
Toggle Clipetty mode in all buffers.
With prefix ARG, enable Global Clipetty mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Clipetty mode is enabled in all buffers where
`(lambda nil (clipetty-mode 1))' would do it.
See `clipetty-mode' for more information on Clipetty mode.

\(fn &optional ARG)" t nil)

(autoload 'clipetty-kill-ring-save "clipetty" "\
Enables Clipetty just for this save.
It can be annoying to have Clipetty overwrite your system
clipboard every time you kill something.  This function wraps
Clipetty around the `kill-ring-save' function and can be invoked
explicitly.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "clipetty" '("clipetty-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; clipetty-autoloads.el ends here
