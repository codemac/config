(in-package :cl-user)
(defpackage net.codemac.stumpwm.modeline
  (:use :cl :stumpwm))

(in-package :net.codemac.stumpwm.modeline)

(setf *time-modeline-string* "%Y-%m-%e %H:%M:%S")

(let ((systray-padding (make-string 10 :initial-element #\Space))
      (date "%d")
      (current-group "%n")
      (groups "%g")
      (hidden-windows "%v"))
  (setf *screen-mode-line-format*
	(list date " | " current-group " | " hidden-windows "^>" systray-padding)))


(setf *mode-line-border-width* 0)
(setf *mode-line-background-color* "#000000")
(setf *mode-line-foreground-color* "#eeeeee")
(setf *mode-line-position* :bottom)

(enable-mode-line (current-screen) (current-head) t)
