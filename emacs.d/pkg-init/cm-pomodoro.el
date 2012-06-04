;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's pomo those doros.

(defun pomo-fast ()
  (interactive)
  (setq pomodoro-work-time 5)
  (setq pomodoro-short-break 1)
  (setq pomodoro-long-break 15)
  (setq pomodoro-set-number 10))

(defun pomo-std ()
  (interactive)
  (setq pomodoro-work-time 25)
  (setq pomodoro-short-break 5)
  (setq pomodoro-long-break 15)
  (setq pomodoro-set-number 4))

(require 'pomodoro)
