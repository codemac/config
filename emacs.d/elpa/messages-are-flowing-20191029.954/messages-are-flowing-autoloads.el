;;; messages-are-flowing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "messages-are-flowing" "messages-are-flowing.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from messages-are-flowing.el

(autoload 'messages-are-flowing-use-and-mark-hard-newlines "messages-are-flowing" "\
Turn on `use-hard-newlines', and make hard newlines visible.
The main use of this is to send \"flowed\" email messages, where
line breaks within paragraphs are adjusted by the recipient's
device, such that messages remain readable on narrow displays.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "messages-are-flowing" '("messages-are-flowing-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; messages-are-flowing-autoloads.el ends here
