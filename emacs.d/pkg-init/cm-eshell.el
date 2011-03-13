;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eshell
;;

(require 'eshell)

(defun cm-eshell-prompt ()
  (concat user-login-name "@" system-name ":"
	  ((lambda (p-lst)
	     (if (> (length p-lst) 4)
		 (concat
		  (mapconcat (lambda (elm) (if (string< "" elm)
					       (substring elm 0 1)
					     ""))
			     (butlast p-lst (- (length p-lst) 3))
			     "/")
		  "/"
		  (mapconcat (lambda (elm) elm)
			     (last p-lst (- (length p-lst) 3))
			     "/"))
	       (mapconcat (lambda (elm) elm)
			  p-lst
			  "/")))
	   (split-string (abbreviate-file-name (eshell/pwd)) "/"))
	  " % "))


(defun eshell-new (name)
  "Create a shell buffer named NAME."
  (interactive "sEshell Name: ")
  (setq name (concat "*eshell:" name "*"))
  (eshell)
  (rename-buffer name))


(autoload 'ansi-color "ansi-color" t nil)

(defun cm-eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
			      eshell-last-output-end))

(setq eshell-directory-name "~/.emacs.d/eshell")
(setq eshell-prompt-function 'cm-eshell-prompt)
(setq eshell-prompt-regexp "^[^[:space:]]+? [%#] ")

(defun cm-eshell-mode-hook ()
  (add-to-list 'eshell-output-filter-functions 'cm-eshell-handle-ansi-color))

(add-hook 'eshell-mode-hook 'cm-eshell-mode-hook)

(provide 'cm-eshell)
