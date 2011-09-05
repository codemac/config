;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eshell
;;

(require 'eshell)
(load-file "~/.emacs-priv.el")
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

(defalias 'enew 'eshell-new)

(put 'eshell 'disabled "Use eshell-new instead!\n")
(autoload 'ansi-color "ansi-color" t nil)

(defun cm-eshell-handle-ansi-color ()
  (ansi-color-apply-on-region eshell-last-output-start
			      eshell-last-output-end))

(setq eshell-directory-name "~/.emacs.d/eshell")
(setq eshell-prompt-function 'cm-eshell-prompt)
(setq eshell-prompt-regexp "^[^%#$\n]+ [%#$] ")
(setenv "EDITOR" "emacsclient")
(setenv "P4USER" "jmickey")
(setenv "P4PORT" cm-ironport-p4port)
(setenv "P4CONFIG" "P4ENV")

;; Stolen from http://www.emacswiki.org/cgi-bin/wiki.pl/EshellEnhancedLS
(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

(defun cm-eshell-mode-hook ()
  (add-to-list 'eshell-output-filter-functions 'cm-eshell-handle-ansi-color))

(add-hook 'eshell-mode-hook 'cm-eshell-mode-hook)

(provide 'cm-eshell)
