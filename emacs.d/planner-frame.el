;;; alert-frame.el --- Devote a frame to ERC alerts

;; Copyright (C) 2009 Jeff Mickey

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


;;; User Variables

(defcustom alert-frame-parameters nil
  "Frame parameters used by `alert-other-frame' to create an alert frame.
This should be an alist for Emacs, or a plist for XEmacs."
  :group 'erc
  :type (if (featurep 'xemacs)
	    '(repeat (list :inline t :format "%v"
			   (symbol :tag "Property")
			   (sexp :tag "Value")))
	  '(repeat (cons :format "%v"
			 (symbol :tag "Parameter")
			 (sexp :tag "Value")))))

(defcustom planner-exit-alert-hook nil
  "Hook called when exiting Planner."
  :group 'erc
  :type 'hook)

(defcustom planner-suspend-alert-hook nil
  "Hook called when suspending Alert."
  :group 'erc
  :type 'hook)

;;; Default Keybindings

(define-key alert-mode-map "\C-cz" 'alert-suspend-alert)
(define-key alert-mode-map "\C-cx" 'alert-exit-alert)

;;; Internal Functions

(defun alert-select-frame-set-input-focus (frame)
  "Select `alert-frame', raise it, and set input focus, if possible."
  ;;; taken verbatim from `gnus-select-frame-set-input-focus'
  (cond ((featurep 'xemacs)
	 (raise-frame frame)
	 (select-frame frame)
	 (focus-frame frame))
	;; The function `select-frame-set-input-focus' won't set
	;; the input focus under Emacs 21.2 and X window system.
	;;((fboundp 'select-frame-set-input-focus)
	;; (defalias 'gnus-select-frame-set-input-focus
	;;   'select-frame-set-input-focus)
	;; (select-frame-set-input-focus frame))
	(t
	 (raise-frame frame)
	 (select-frame frame)
	 (cond ((and (eq window-system 'x)
		     (fboundp 'x-focus-frame))
		(x-focus-frame frame))
	       ((eq window-system 'w32)
		(w32-focus-frame frame)))
	 (when focus-follows-mouse
	   (set-mouse-position frame
			       (1- (frame-width frame)) 0)))))

(defun alert-frame-or-window-display-name (object)
  "Given a frame or window, return the associated display name.
Return nil otherwise."
  (if (featurep 'xemacs)
      (device-connection (dfw-device object))
    (if (or (framep object)
	    (and (windowp object)
		 (setq object (window-frame object))))
	(let ((display (frame-parameter object 'display)))
	  (if (and (stringp display)
		   ;; Exclude invalid display names.
		   (string-match "\\`[^:]*:[0-9]+\\(\\.[0-9]+\\)?\\'"
				 display))
	      display)))))

(defun alert-buffer-p (buf)
  (with-current-buffer buf
    (string-match mode-name "alert")))

(defun alert-alive-p ()
  "Determine whether Alert is running" 
  ;; alert is running iff there is a buffer anywhere whose mode is
  ;; `alert-mode'.
  (let ((buf-list (buffer-list))
	(found nil))
    (while (and buf-list (not found))
      (let ((buf (car buf-list)))
	(when (alert-buffer-p buf)
	  (setq found t)))
      (setq buf-list (cdr buf-list)))
    found))

;;; Internal Variables

(defvar alert-most-recent-buffer nil
  "The most recent alert buffer that has been visited.")

(defvar alert-frame nil
  "A frame object which will be created by `plan-other-frame'.")

;;; Public Functions

;;;###autoload
(defun plan-other-frame (&optional force-days display)
  "Like `plan', but does so in a new frame.

This will call `plan' and pop up a Alert frame; the value of
FORCE-DAYS is passed to `plan'.  The optional second argument
DISPLAY should be a standard display string such as \"unix:0\" to
specify where to pop up a frame.  If DISPLAY is omitted or the
function `make-frame-on-display' is not available, the current
display is used."
;; Docstring taken from docstring of `gnus-other-frame'.
  (interactive "P")
  (if (fboundp 'make-frame-on-display)
      (unless display
	(setq display (alert-frame-or-window-display-name (selected-frame))))
    (setq display nil))
  (let ((alive (alert-alive-p)))
    (unless (and alive
		 (catch 'found
		   (walk-windows
		    (lambda (window)
		      (when
			  (and
			   (or (not display)
			       (equal display
				      (alert-frame-or-window-display-name
				       window)))
			   (with-current-buffer (window-buffer window)
			     (string-match "\\`alert-"
					   (symbol-name major-mode))))
			(alert-select-frame-set-input-focus
			 (setq alert-frame (window-frame window)))
			(select-window window)
			(throw 'found t)))
		    'ignore t)))
      (alert-select-frame-set-input-focus
       (setq alert-frame
	     (if display
		 (make-frame-on-display display alert-frame-parameters)
	       (make-frame alert-frame-parameters))))
      (if alive
	  (switch-to-buffer alert-most-recent-buffer)
	(plan force-days)))))

;;;###autoload
(defun alert-exit-alert ()
  "Save and then close all Alert buffers, delete the frame that
Alert created, and run the hook `alert-exit-alert-hook'."
  (interactive)
  (alert-save-buffers)
  (when (and (frame-live-p alert-frame)
	     (cdr (frame-list)))
    (delete-frame alert-frame))
  (dolist (buf (buffer-list))
    (when (alert-buffer-p buf)
      (kill-buffer buf)))
  (setq alert-frame nil)
  (run-hooks 'alert-exit-alert-hook))

;;;###autoload
(defun alert-suspend-alert ()
  "Delete the frame that was created for the Alert, then run the
hook `alert-suspend-alert-hook'."
  (interactive)
  (when (and (frame-live-p alert-frame)
	     (cdr (frame-list)))
    (delete-frame alert-frame))
  (run-hooks 'alert-suspend-alert-hook))

;;; Export Statements
(provide 'alert-frame)

;;; alert-frame.el ends here
