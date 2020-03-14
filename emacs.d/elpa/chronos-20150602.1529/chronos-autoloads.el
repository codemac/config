;;; chronos-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "chronos" "chronos.el" (0 0 0 0))
;;; Generated autoloads from chronos.el

(autoload 'chronos-add-timer "chronos" "\
Add a timer to expire at time TIME with message MSG.

TIME can be absolute or relative (positive countdown or negative
countup) to now or (with the prefix argument) the selected
timer.

\(fn TIME MESSAGE PREFIX)" t nil)

(autoload 'chronos-add-timers-from-string "chronos" "\
Add a timer (or timers) based on TIMER-STRING.

TIMER-STRING consists of timer specifications separated by `+'s.

Timer specifications consist of an expiry specification and a
message separated by a `/'.

If the prefix argument is selected, the (first) timer will be
relative to the selected timer, otherwise current time.

Subsequent timers in the string will be relative to the previous timer.

A list of timers ((exp msg) ...) is returned.

\(fn TIMERS-STRING PREFIX)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "chronos" '("chronos-")))

;;;***

;;;### (autoloads nil nil ("chronos-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; chronos-autoloads.el ends here
