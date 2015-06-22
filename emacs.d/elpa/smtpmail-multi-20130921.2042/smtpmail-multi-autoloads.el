;;; smtpmail-multi-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "smtpmail-multi" "smtpmail-multi.el" (21889
;;;;;;  60458 988375 796000))
;;; Generated autoloads from smtpmail-multi.el

(autoload 'smtpmail-multi-change "smtpmail-multi" "\
Change the smtp settings to match the settings for ACCOUNT in `smtpmail-multi-accounts'.

\(fn ACCOUNT)" nil nil)

(autoload 'smtpmail-multi-get-accounts "smtpmail-multi" "\
Returns the SMTP accounts associated with the current buffer according to `smtpmail-multi-associations'.
The account details associated with each account name are stored in `smtpmail-multi-accounts'.
If there is no SMTP account associated with the current buffer, return `smtpmail-multi-default-account'
instead.

\(fn)" nil nil)

(autoload 'smtpmail-multi-send-it "smtpmail-multi" "\
Send mail using smtp server selected by the `smtpmail-multi-select' function.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; smtpmail-multi-autoloads.el ends here
