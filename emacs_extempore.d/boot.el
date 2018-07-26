(use-package extempore
  )

(keyboard-translate ?\C-x ?\C-t)
(keyboard-translate ?\C-t ?\C-x)

(global-set-key
 (kbd "C-t") 
 (lambda ()
   (interactive)
   (run-with-timer 
    0.3 nil 
    (lambda ()
      ;; Assuming these are the default values
      (setq visible-bell nil)
      (setq ring-bell-function 'ignore)))
   (setq visible-bell t)
   (setq ring-bell-function nil)
   (error "Don't press that button.")))
