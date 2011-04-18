;;; xgtags.el --- gtags facility for Emacs

;;
;; Copyright (c) 2006 Marco Gidde
;;
;; Author: Marco Gidde <marco.gidde@tiscali.de>
;; Maintainer: Marco Gidde <marco.gidde@tiscali.de>
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: xgtags.el,v 1.35 2006/03/19 12:57:51 mg Exp $

;;; Commentary:
;;
;; xgtags.el provides an interface to the GLOBAL
;; (http://www.gnu.org/software/global) cross-refencing tools. While
;; gtags.el, that comes with the GLOBAL distribution, is more a
;; replacement for emacs' own find-tag/pop-tag-mark facility with some
;; extra stuff, xtags.el tries to permit the same functionality as
;; gtags.el, but behaves more like xcscope.el, the emacs interface for
;; cscope (http://cscope.sourceforge.net).
;;
;; xgtags consists of xgtags-mode, a minor that should work with any
;; major mode, especially the programming modes, and
;; xgtags-select-mode which presents the result of the last query to
;; the user.
;;
;;
;;; Installation
;;
;; To use xgtags copy this file to some place where emacs can find it,
;; if necessary add the path you chose to the load-path variable. In
;; your .emacs add the line
;;
;;	(require 'xgtags)
;;
;; In any buffer you can now toggle xgtags-mode by calling the
;; interactive command with same name. Since this is a bit annoying,
;; you might choose to turn it on for some specific modes. For c-mode
;; you can add something like the following snippet to your .emacs
;; file. Other modes provide similar hooks.
;;
;;         (add-hook 'c-mode-common-hook
;;                   (lambda ()
;; 		        (xgtags-mode 1)))
;;
;; After that you can use the predefined keybindings to query your
;; GLOBAL database. Call 'describe-function RET xgtags-mode' to get an
;; overview of those bindings.
;;
;;
;;; TODO:
;; - some support for include files
;; - key binding for xgtags-query-replace-regexp
;; - BUG: return in selection list
;; - show the database in the *xgtags* buffer
;; - cutomizable keybindings?
;;


;;; Code

;; (require 'cl)

(defconst xgtags-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version)
  "Whether we are running XEmacs/Lucid Emacs")

(defgroup xgtags nil
  "Using gtags and global for crossrefencing"
  :group 'tools)

(defun xgtags-set-overwrite-global-bindings (flag &optional keymap)
  (let ((map (or keymap (and (boundp 'xgtags-mode-map) xgtags-mode-map))))
    (cond
     ((not (keymapp map)))
     (flag (define-key map "\e*" 'xgtags-pop-stack)
	   (define-key map "\e." 'xgtags-find-tag))
     (t (define-key map "\e*" nil)
	(define-key map "\e." nil)))))

(defun xgtags-set-overwrite-complete-bindings (flag &optional keymap)
  (let ((map (or keymap (and (boundp 'xgtags-mode-map) xgtags-mode-map))))
    (cond
     ((not (keymapp map)))
     (flag (define-key map "\e\t" 'xgtags-complete-tag))
     (t (define-key map "\e\t" nil)))))

(defun xgtags-set-support-mouse (flag &optional keymap)
  (let ((map (or keymap (and (boundp 'xgtags-mode-map) xgtags-mode-map))))
    (cond
     ((not (keymapp map)))
     (xgtags-running-xemacs
      (define-key map 'button3 (when flag 'xgtags-pop-stack))
      (define-key map 'button2 (when flag 'xgtags-find-tag-by-event)))
     (t
      (define-key map [mouse-3] (when flag 'xgtags-pop-stack))
      (define-key map [mouse-2] (when flag 'xgtags-find-tag-by-event))))))

;;;
;;; faces
;;;

(defface xgtags-match-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight function name in the *xgtags* buffer."
  :group 'xgtags)

(defface xgtags-file-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face used to highlight file name in the *xgtags* buffer."
  :group 'xgtags)

(defface xgtags-line-number-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face used to highlight line number in the *xgtags* buffer."
  :group 'xgtags)

(defface xgtags-line-face
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *xgtags* buffer."
  :group 'xgtags)

;;;
;;; customization
;;;

(defcustom xgtags-overwrite-global-bindings t
  "Should the default keybindings for tags, that is \M-. e.a., be
overwritten with the corresponding xgtags functionality?"
  :type 'boolean
  :group 'xgtags
  :set (lambda (symbol value)
	 (set symbol value)
	 (xgtags-set-overwrite-global-bindings value)))

(defcustom xgtags-overwrite-complete-bindings nil
  "Should the default keybindings for symbol completition, that is
\M-<TAB>, be overwritten with the corresponding xgtags functionality?"
  :type 'boolean
  :group 'xgtags
  :set (lambda (symbol value)
	 (set symbol value)
	 (xgtags-set-overwrite-complete-bindings value)))

(defcustom xgtags-support-mouse t
  "Should there be some default bindings for mouse-2 and mouse-3."
  :type 'boolean
  :group 'xgtags
  :set (lambda (symbol value)
	 (set symbol value)
	 (xgtags-set-support-mouse value)))

(defcustom xgtags-read-only nil
  "When set to true, new files are opened in read only mode."
  :type 'boolean
  :group 'xgtags)

(defcustom xgtags-update-db nil
  "Use this option when you like to automatically update your database
before calling 'global'. The options are no update at all, a command
to be called or an elisp function, that will be called with the
current rootdir."
  :type '(radio (const :tag "No update.")
		(string :tag "Command and options.")
		(function :tag "Elisp function to be called."))
  :group 'xgtags)

(defcustom xgtags-find-multiple-db nil
  "When bound to a function this function is called with one argument,
namely the current directory, and should return a list of directories
with GTAGS databases. All databases are searched one after another."
  :type 'function
  :group 'xgtags)

(defcustom xgtags-kill-buffers t
  "Should the current buffer be killed when the stack entry is popped?
Be careful: even buffers not opened by xgtags itself will be killed!"
  :type 'boolean
  :group 'xgtags)

(defcustom xgtags-select-buffer-name "*xgtags*"
  "Name to use as the select buffer."
  :type 'string
  :group 'xgtags)

(defcustom xgtags-rootdir nil
  "Root directory of source tree."
  :type 'string
  :group 'xgtags)

(defconst xgtags-symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")
(defconst xgtags-definition-regexp "#[ \t]*define[ \t]+\\|ENTRY(\\|ALTENTRY("
  "Regexp matching tag definition name.")
(defconst xgtags-tag-regexp "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([^\n]+\\)"
  "Regex matching the current output line for a tag.")
(defconst xgtags-file-regexp "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\n]+\\)"
  "Regex matching the current output line for a tag.")

(defvar xgtags-stack nil
  "Stack for tag browsing.")
(defvar xgtags-completition-table nil
  "xgtags complete obarray.")
(defvar xgtags-history-list nil
  "xgtags history list.")
(defvar xgtags-minor-mode-text " xgtags"
  "xgtags text to be shown in the mode line.")

(defvar xgtags-mode-map (let ((keymap (make-sparse-keymap))
			     (sub-keymap (make-sparse-keymap)))
			 (define-key keymap "\C-cw" sub-keymap)
			 (define-key sub-keymap "d" 'xgtags-find-tag)
			 (define-key sub-keymap "c" 'xgtags-find-rtag)
			 (define-key sub-keymap "s" 'xgtags-find-symbol)
			 (define-key sub-keymap "g" 'xgtags-find-with-grep)
			 (define-key sub-keymap "i" 'xgtags-find-with-idutils)
			 (define-key sub-keymap "n" 'xgtags-select-next-tag)
			 (define-key sub-keymap "p" 'xgtags-select-prev-tag)
			 (define-key sub-keymap "\t" 'xgtags-make-complete-list)
			 (define-key sub-keymap "u" 'xgtags-pop-stack)
			 ;;   (define-key sub-keymap "" 'xgtags-find-tag-from-here)
			 (define-key sub-keymap "f" 'xgtags-find-file)
			 ;;   (define-key sub-keymap "" 'xgtags-display-browser)
			 (when xgtags-overwrite-global-bindings
			   (xgtags-set-overwrite-global-bindings t keymap))
			 (when xgtags-overwrite-complete-bindings
			   (xgtags-set-overwrite-complete-bindings t keymap))
			 (when xgtags-support-mouse
			   (xgtags-set-support-mouse t keymap))
			 keymap)
  "Keymap used in xgtags mode.")


(defvar xgtags-select-mode-name "xgtags-select"
  "xgtags-select text to be shown in the mode line.")
(defvar xgtags-select-tags nil)

(defvar xgtags-select-mode-map (make-sparse-keymap)
  "Keymap used in xgtags select mode.")

(define-key xgtags-select-mode-map "\e*" 'xgtags-pop-stack)
(if (not xgtags-running-xemacs) nil
  (define-key xgtags-select-mode-map 'button3 'xgtags-pop-stack)
  (define-key xgtags-select-mode-map 'button2 'xgtags-select-tag-by-event))
(if xgtags-running-xemacs nil
  (define-key xgtags-select-mode-map [mouse-3] 'xgtags-pop-stack)
  (define-key xgtags-select-mode-map [mouse-2] 'xgtags-select-tag-by-event))
(define-key xgtags-select-mode-map "\^?" 'scroll-down)
(define-key xgtags-select-mode-map " " 'scroll-up)
(define-key xgtags-select-mode-map "\C-b" 'scroll-down)
(define-key xgtags-select-mode-map "\C-f" 'scroll-up)
(define-key xgtags-select-mode-map "k" 'previous-line)
(define-key xgtags-select-mode-map "j" 'next-line)
(define-key xgtags-select-mode-map "p" 'previous-line)
(define-key xgtags-select-mode-map "n" 'next-line)
(define-key xgtags-select-mode-map "q" 'xgtags-pop-stack)
(define-key xgtags-select-mode-map "u" 'xgtags-pop-stack)
(define-key xgtags-select-mode-map "\C-t" 'xgtags-pop-stack)
(define-key xgtags-select-mode-map "\C-m" 'xgtags-select-tag)
(define-key xgtags-select-mode-map "\e." 'xgtags-select-tag)

;;
;; utility
;;

(defmacro with-xgtags-environment (&rest body)
  `(let ((process-environment (copy-alist process-environment)))
     (when xgtags-rootdir
       (setenv "GTAGSROOT" xgtags-rootdir))
     ,@body))

(defun xgtags-non-empty (string)
  "If string is empty return nil else return the string itself"
  (unless (equal string "")
    string))

(defun xgtags-match-string (n)
  (buffer-substring-no-properties (match-beginning n) (match-end n)))

(defun xgtags-token-at-point ()
  "Return a default tag to search for, based on the text at point."
  (save-excursion
    (cond
     ((looking-at "[0-9A-Za-z_]")
      (while (looking-at "[0-9A-Za-z_]")
	(forward-char -1))
      (forward-char 1))
     (t
      (while (looking-at "[ \t]")
	(forward-char 1))))
    (when (and (bolp) (looking-at xgtags-definition-regexp))
      (goto-char (match-end 0)))
    (when (looking-at xgtags-symbol-regexp)
      (xgtags-match-string 0))))

(defun xgtags-file-at-point ()
  "Return a default tag to search for, based on the text at point."
  (save-excursion
    (cond
     ((looking-at "[0-9A-Za-z_\.]")
      (while (looking-at "[0-9A-Za-z_\.]")
	(forward-char -1))
      (forward-char 1)))
    (when (looking-at "[0-9A-Za-z_\.]+")
      (xgtags-match-string 0))))

(defun xgtags-is-function ()
  "Is it a function?"
  (save-excursion
    (while (and (not (eolp)) (looking-at "[0-9A-Za-z_]"))
      (forward-char 1))
    (while (and (not (eolp)) (looking-at "[ \t]"))
      (forward-char 1))
    (looking-at "(")))

(defun xgtags-is-definition ()
  "Is it a definition?"
  (save-excursion
    (if (or (and (string-match "\.java$" buffer-file-name)
		 (looking-at "[^(]+([^)]*)[ \t]*{"))
	    (bolp))
	t
      (forward-word -1)
      (cond
       ((looking-at "define")
	(forward-char -1)
	(while (and (not (bolp)) (looking-at "[ \t]"))
	  (forward-char -1))
	(and (bolp) (looking-at "#")))
       ((looking-at "ENTRY\\|ALTENTRY")
	(bolp))))))

(defun xgtags-get-buffer ()
  (let ((buffer (get-buffer xgtags-select-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create xgtags-select-buffer-name))
      ;; using with-xgtags-buffer should be possible, but is might be
      ;; better to avoid recursive xgtags-get-buffer calls
      (save-excursion
	(set-buffer buffer)
	(when xgtags-stack
	  (xgtags-select-show-tags (xgtags-stack-current-tags (car xgtags-stack))))
	(xgtags-select-mode)))
    buffer))

(defmacro with-xgtags-buffer (&rest body)
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (xgtags-get-buffer)))
       (when ,buffer
	 (save-excursion
	   (set-buffer ,buffer)
	   (let ((buffer-read-only nil))
	     ,@body))))))


;;;
;;; stack handling
;;;

(defun xgtags-make-stack-entry (buffer point tags xgtags-point)
  (vector buffer point tags xgtags-point nil))

(defun xgtags-stack-entry-buffer (entry)
  (aref entry 0))

(defun xgtags-stack-entry-point (entry)
  (aref entry 1))

(defun xgtags-stack-entry-tags (entry)
  (aref entry 2))

(defun xgtags-stack-entry-xgtags-point (entry)
  (aref entry 3))

(defun xgtags-stack-current-tags (entry)
  (aref entry 4))

(defun xgtags-stack-entry-set-current-tags (entry obj)
  (aset entry 4 obj))

(defun xgtags-update-minor-mode-text ()
  (let ((length (length xgtags-stack)))
    (if (zerop length)
	(setq xgtags-minor-mode-text " xgtags")
      (setq xgtags-minor-mode-text (format " xgtags[%d]" length)))))

(defun xgtags-push-context ()
  "push current context to stack"
  (let* ((content (with-xgtags-buffer
		   (cons xgtags-select-tags (point))))
	 (stack-entry (xgtags-make-stack-entry (current-buffer) (point-marker)
					      (car content) (cdr content))))
    (push stack-entry xgtags-stack)
    (xgtags-update-minor-mode-text)
    stack-entry))

(defun xgtags-pop-context ()
  "Pop context from stack."
  (let ((entry (pop xgtags-stack)))
    (when entry
      (let ((tags (xgtags-stack-entry-tags entry))
	    (point (xgtags-stack-entry-xgtags-point entry)))
	(xgtags-update-minor-mode-text)
	(with-xgtags-buffer
	 (when tags
	   (xgtags-select-show-tags tags)
	   (goto-char point)
	   (when (get-buffer-window (current-buffer))
	     (set-window-point (get-buffer-window (current-buffer)) (point)))
	   (xgtags-select-update-mode-text (get-text-property point
							     'xgtags-tag))))
	entry))))

(defun xgtags-pop-stack ()
  "Move to previous point on the stack."
  (interactive)
  (let ((delete (and xgtags-kill-buffers 
		     (not (xgtags-exist-in-stack (current-buffer)))))
	(context (xgtags-pop-context)))
    (if (not context)
	(message "The tags stack is empty.")
      (when delete
	(kill-buffer (current-buffer)))
      (switch-to-buffer (xgtags-stack-entry-buffer context))
      (goto-char (xgtags-stack-entry-point context)))))

(defun xgtags-exist-in-stack (buffer)
  "If this buffer exists on the xgtags stack."
  (memq buffer (mapcar 'xgtags-stack-entry-buffer xgtags-stack)))


;;;
;;; xgtags item
;;;

(defun xgtags-make-tag (search file line match)
  (vector search file line match))

(defun xgtags-tag-search (tag)
  (aref tag 0))

(defun xgtags-tag-file (tag)
  (aref tag 1))

(defun xgtags-tag-line (tag)
  (aref tag 2))

(defun xgtags-tag-match (tag)
  (aref tag 3))


;;
;; interactive command
;;

(defun xgtags-visit-rootdir ()
  "Tell tags commands the root directory of source tree."
  (interactive)
  (when (equal xgtags-rootdir nil)
    (with-temp-buffer
      (setq xgtags-rootdir
	    (if (zerop (call-process "global" nil t nil "-pr"))
		(file-name-as-directory (buffer-substring (point-min)
							  (1- (point-max))))
	      default-directory))))
  (let ((input (read-file-name "Visit root directory: "
			       xgtags-rootdir xgtags-rootdir t)))
    (unless (equal "" input)
      (if (not (file-directory-p input))
	  (message "%s is not directory." input)
	(setq xgtags-rootdir (expand-file-name input))))))

(defun xgtags-find-tag ()
  "Input tag name and move to the definition."
  (interactive)
  (let* ((tagname (xgtags-token-at-point))
	 (prompt (if tagname
		     (concat "Find tag: (default " tagname ") ")
		   "Find tag: "))
	 (input (completing-read prompt xgtags-completition-table
				 nil nil nil xgtags-history-list)))
    (xgtags-goto-tag (or (xgtags-non-empty input) tagname) "")))

(defun xgtags-find-rtag ()
  "Input tag name and move to the referenced point."
  (interactive)
  (let* ((tagname (xgtags-token-at-point))
	 (prompt (if tagname
		     (concat "Find tag (reference): (default " tagname ") ")
		   "Find tag (reference): "))
	 (input (completing-read prompt xgtags-completition-table
				 nil nil nil xgtags-history-list)))
    (xgtags-goto-tag (or (xgtags-non-empty input) tagname) "r")))

(defun xgtags-find-symbol ()
  "Input symbol and move to the locations."
  (interactive)
  (let* ((tagname (xgtags-token-at-point))
	 (prompt (if tagname
		     (concat "Find symbol: (default " tagname ") ")
		   "Find symbol: "))
	 (input (completing-read prompt xgtags-completition-table
				 nil nil nil xgtags-history-list)))
    (xgtags-goto-tag (or (xgtags-non-empty input) tagname) "s")))

;; find with grep or id-utils.
(defun xgtags-find-with (flag)
  (let* ((tagname (xgtags-token-at-point))
	 (prompt (if tagname
		     (concat "Find pattern: (default " tagname ") ")
		   "Find pattern: "))
	 (input (completing-read prompt xgtags-completition-table
				 nil nil nil xgtags-history-list)))
    (xgtags-goto-tag (or (xgtags-non-empty input) tagname) flag)))


(defun xgtags-find-pattern ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (xgtags-find-with-grep))

(defun xgtags-find-with-grep ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (xgtags-find-with "g"))

(defun xgtags-find-with-idutils ()
  "Input pattern, search with id-utils(1) and move to the locations."
  (interactive)
  (xgtags-find-with "I"))

(defun xgtags-find-file ()
  (interactive)
  "Input pattern and move to the top of the file."
  (let* ((tagname (xgtags-file-at-point))
	 (prompt (if tagname
		     (concat "Find file: (default " tagname ") ")
		   "Find file: "))
	 (input (completing-read prompt nil nil nil nil nil)))
    (xgtags-goto-tag (or (xgtags-non-empty input) tagname) "P")))

(defun xgtags-parse-file ()
  "Input file name, parse it and show object list."
  (interactive)
  (let ((input (read-file-name "Parse file: "
			       nil nil t
			       (file-name-nondirectory buffer-file-name))))
    (xgtags-goto-tag (xgtags-non-empty input) "f")))

(defun xgtags-find-tag-from-here ()
  "Get the expression as a tagname around here and move there."
  (interactive)
  (let ((tagname (xgtags-token-at-point))
	(flag (cond ((not (xgtags-is-function)) "s")
		    ((xgtags-is-definition) "r")
		    (t ""))))
    (when tagname
      (xgtags-goto-tag tagname flag))))

; This function doesn't work with mozilla.
; But I will support it in the near future.
; MG: it works for me
(defun xgtags-display-browser ()
  "Display current screen on hypertext browser."
  (interactive)
  (unless (and (not (equal (point-min) (point-max)))
	       buffer-file-name)
    (let ((lno (save-excursion
		 (end-of-line)
		 (max 1 (count-lines (point-min) (point))))))
      (with-temp-buffer
	(unless (zerop (call-process "gozilla"  nil t nil
				     (concat "+" (number-to-string lno))
				     buffer-file-name))
	  (message "%s" (buffer-string)))))))

(defun xgtags-find-tag-by-event (event)
  "Get the expression as a tagname around here and move there."
  (interactive "e")
  (let (tagname flag)
    (if (zerop (count-lines (point-min) (point-max)))
        (progn (setq tagname "main")
	       (setq flag ""))
      (if xgtags-running-xemacs
	  (goto-char (event-point event))
	(select-window (posn-window (event-end event)))
        (set-buffer (window-buffer (posn-window (event-end event))))
        (goto-char (posn-point (event-end event))))
      (setq tagname (xgtags-token-at-point))
      (setq flag (cond ((not (xgtags-is-function)) "s")
		       ((xgtags-is-definition) "r")
		       (t  ""))))
    (when tagname
      (xgtags-goto-tag tagname flag))))

(defun xgtags-map-tags (func)
  "Maps over all tags in the *xgtags* buffer, jumps to the tag and
funcalls the function with the match as argument."
  (let ((buffer (xgtags-get-buffer)))
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (while (xgtags-select-next-tag)
      (let* ((tag (save-excursion
		    (set-buffer buffer)
		    (get-text-property (point) 'xgtags-tag)))
	     (match (xgtags-tag-search tag)))
	(funcall func match)))))

;; (defun xgtags-test ()
;;   (interactive)
;;   (xgtags-map-tags
;;    (lambda (match)
;;      (message "foo: %s" match)
;;      (when (search-forward-regexp match
;; 				  (save-excursion (end-of-line) (point))
;; 				  t)
;;        (goto-char (match-beginning 0))
;;        (set-mark (match-end 0))
;;        (y-or-n-p "Weiter? ")))))

(defun xgtags-query-replace-regexp (to-string)
  "Run over the current *xgtags* buffer and to `query-replace-regexp'
for each tag."
  (interactive
   (list (read-from-minibuffer (format "Replace <%s> with: " "current tag")
			       nil nil nil
			       query-replace-to-history-variable nil t)))
  (xgtags-map-tags
   (lambda (match)
     (message "foo: %s" match)
     (query-replace-regexp match to-string nil
			   (point)
			   (save-excursion (end-of-line) (point))))))

(defun xgtags-select-next-prev-tag (arg)
  "Select the next or previous tag in the previous select buffer."
  (unless arg
    (setq arg 1))
  (let* ((lines (abs (or arg 1)))
	 (direction (if (< (or arg 1) 0) -1 1))
	 (current (current-buffer))
	 (buffer (xgtags-get-buffer)))
    (set-buffer buffer)
    (let ((old-point (point)))
      (dotimes (i lines)
	(unless (and (zerop (forward-line direction))
		     (not (equal (point) (point-min)))
		     (not (equal (point) (point-max)))
		     (xgtags-find-near-select-tag (< direction 0)))
	  (goto-char old-point)
	  (when (get-buffer-window buffer)
	    (set-window-point (get-buffer-window buffer) (point)))
	  (set-buffer current)
	  (error "The %s of the *xgtags* buffer has been reached."
		 (if (> direction 0) "end" "beginning"))))
      (xgtags-select-tag))))

(defun xgtags-select-next-tag (&optional arg)
  "Select the next tag in the previous select buffer."
  (interactive "p")
  (xgtags-select-next-prev-tag arg))

(defun xgtags-select-prev-tag (&optional arg)
  "Select the previous tag in the previous select buffer."
  (interactive "p")
  (xgtags-select-next-prev-tag (- arg)))

(defun xgtags-select-tag-by-event (event)
  "Select a tag in [XGTAGS SELECT MODE] and move there."
  (interactive "e")
  (if xgtags-running-xemacs
      (goto-char (event-point event))
    (select-window (posn-window (event-end event)))
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event))))
  (xgtags-select-tag))

;;
;; common function
;;

;; goto tag's point
(defun xgtags-goto-tag (tagname flag)
  (let* ((window (selected-window))
	 (file-name (buffer-file-name))
	 (buffer-dir (and file-name (file-name-directory file-name)))
	 (buffer (xgtags-get-buffer)))
    (message "Searching %s ..." tagname)
    (let* ((stack-entry (xgtags-push-context))
	   (tags (xgtags-call-global buffer-dir flag tagname))
	   (num-tags (length tags)))
      (xgtags-stack-entry-set-current-tags stack-entry tags)
      (message "Searching %s done" tagname)
      (cond
       ((eq num-tags 0)
	(message (cond
		  ((equal flag "P") "%s: path not found")
		  ((equal flag "g") "%s: pattern not found")
		  ((equal flag "I") "%s: token not found")
		  ((equal flag "s") "%s: symbol not found")
		  (t "%s: tag not found"))
		 tagname)
	(xgtags-pop-context))
       (t
	(set-buffer buffer)
	(goto-char (point-min))
	(xgtags-select-tag)
	(when (> num-tags 1)
	  (pop-to-buffer buffer)
	  (select-window window)))))))

(defun xgtags-call-global (buffer-dir flag tagname)
  (let ((tags nil))
    (xgtags-do-in-all-directories
     buffer-dir
     (lambda (dir)
       (when dir
	 (message "search in %s" dir))
       (let ((xgtags-rootdir (file-name-as-directory dir)))
	 (with-xgtags-environment
	  (when xgtags-update-db
	    (xgtags-update-db xgtags-rootdir))
	  (with-temp-buffer
	    (if (zerop (call-process "global" nil t nil
				     (concat "-ax" flag) tagname))
		(setq tags (append tags (xgtags-collect-tags-in-buffer)))
	      (message (buffer-substring (point-min)(1- (point-max))))))))))
    (with-xgtags-buffer
     (xgtags-select-show-tags tags))
    tags))

(defun xgtags-do-in-all-directories (buffer-dir func)
  (dolist (dir (or (and buffer-dir
			xgtags-find-multiple-db
			(funcall xgtags-find-multiple-db buffer-dir))
		   (list xgtags-rootdir)))
    (funcall func dir)))

(defun xgtags-update-db (directory)
  (cond
   ((stringp xgtags-update-db)
    (let ((default-directory directory)
	  (split (split-string xgtags-update-db)))
      (apply #'call-process (car split) nil nil nil (cdr split))))
   ((functionp xgtags-update-db)
    (funcall xgtags-update-db directory))
   (t (error "xgtags-update-db has an unsupported type."))))

(defun xgtags-collect-tags-in-buffer ()
  "This function searches the current buffer for tag items and returns
a list with those."
  (save-excursion
    (goto-char (point-min))
    (let ((tags nil))
      (while (not (eobp))
	(cond
	 ((looking-at xgtags-tag-regexp)
	  (let ((search (xgtags-match-string 1))
		(line (string-to-number (xgtags-match-string 2)))
		(file (xgtags-match-string 3))
		(match (xgtags-match-string 4)))
	    (push (xgtags-make-tag search file line match) tags)))
	 ((looking-at xgtags-file-regexp)
	  (let ((search (xgtags-match-string 1))
		(line (string-to-number (xgtags-match-string 2)))
		(file (xgtags-match-string 3)))
	    (push (xgtags-make-tag search file line nil) tags))))
	(forward-line))
      (nreverse tags))))

(defun xgtags-insert-with-face (string face)
  (let ((start (point)))
    (insert string)
    (put-text-property start (point) 'face face)))

;; select a tag line from lines
(defun xgtags-select-tag ()
  "Select a tag in [XGTAGS SELECT MODE] and move there."
  (interactive)
  ;; get context from current tag line
  (if (not (xgtags-find-near-select-tag))
      (xgtags-pop-context)
    (let* ((tag (get-text-property (point) 'xgtags-tag))
	   (match (xgtags-tag-search tag))
	   (line (xgtags-tag-line tag))
	   (file (xgtags-tag-file tag)))
      ;; move to the context
      (xgtags-select-update-mode-text tag)
      (if xgtags-read-only
	  (find-file-read-only file)
	(find-file file))
      (xgtags-mode 1)
      (goto-line line)
      (let ((found nil)
	    (lines 0))
	(while (and (not found) (< lines 5))
	  (let ((start (save-excursion (forward-line (- lines))
				       (point)))
		(end (save-excursion (forward-line lines)
				     (end-of-line)
				     (point))))
	    (save-excursion
	      (goto-char start)
	      (setq found (search-forward-regexp match end t))))
	  (setq lines (1+ lines)))
	(when found
	  (goto-char (match-beginning 0)))))))

(defun xgtags-find-near-select-tag (&optional backwards)
  "Find the next selectable tag. If there is none at the current line,
step a line forward or backward to find one."
  (beginning-of-line)
  (let ((buffer-window (get-buffer-window (current-buffer))))
    (while (when (not (get-text-property (point) 'xgtags-tag))
	     (zerop (forward-line (and backwards -1)))))
    (when buffer-window
      (set-window-point buffer-window (point)))
    (get-text-property (point) 'xgtags-tag)))

;; make complete list
(defun xgtags-make-complete-list ()
  "Make tag name list for completion."
  (interactive)
  (let* ((file-name (buffer-file-name))
	 (buffer-dir (and file-name (file-name-directory file-name))))
    (message "Making completion list ...")
    (setq xgtags-completition-table (make-vector 511 0))
    (xgtags-do-in-all-directories
     buffer-dir
     (lambda (dir)
       (message "Making completion list in %s" dir)
       (let ((xgtags-rootdir (file-name-as-directory dir)))
	 (with-xgtags-environment
	  (with-temp-buffer
	    (call-process "global" nil t nil "-c")
	    (goto-char (point-min))
	    (while (looking-at xgtags-symbol-regexp)
	      (intern (xgtags-match-string 0) xgtags-completition-table)
	      (forward-line)))))))
    (message "Making completion list ... Done")))

(defun xgtags-complete-tag ()
;;   "Perform tags completion on the text around point.
;; Completes to the set of names listed in the current tags table.
;; The string to complete is chosen in the same way as the default
;; for \\[find-tag] (which see)."
  (interactive)
  (unless xgtags-completition-table
    (error "No xgtags-completition-table created."))
  (while (looking-at "[ \t\n]")
    (backward-char 1))
  (let ((pattern (xgtags-token-at-point))
	(beg (match-beginning 0))
	(end (match-end 0))
	completion)
    (unless pattern
      (error "Nothing to complete."))
    (goto-char end)
    (setq completion (try-completion pattern xgtags-completition-table nil))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg (point))
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (all-completions pattern xgtags-completition-table)))
	   (message "Making completion list...%s" "done")))))

(defun xgtags-select-update-mode-text (tag)
  (let ((pos (xgtags-select-find-tag-position tag)))
    (if pos
	(setq mode-name (format "%s[%d/%d]" 
				xgtags-select-mode-name
				(1+ pos) (length xgtags-select-tags)))
      (setq mode-name (format "%s[%d]" 
			      xgtags-select-mode-name
			      (length xgtags-select-tags))))))

(defun xgtags-select-find-tag-position (tag)
  (let ((pos 0)
	(found nil)
	(tags xgtags-select-tags))
    (while (and tags (not (setq found (eq (car tags) tag))))
      (setq pos (1+ pos))
      (setq tags (cdr tags)))
    (when found
      pos)))

(defun xgtags-select-show-tags (tags)
  "This function gets a list of tag items and inserts them nicely into
the current buffer."
  (setq xgtags-select-tags tags)
  (erase-buffer)
  (let ((current-file nil))
    (dolist (tag tags)
      (let ((search (xgtags-tag-search tag))
	    (file (xgtags-tag-file tag))
	    (line (xgtags-tag-line tag))
	    (match (xgtags-tag-match tag)))
	(unless (equal current-file file)
	  (when current-file
	    (insert "\n"))
	  (setq current-file file)
	  (xgtags-insert-with-face file 'xgtags-file-face)
	  (insert "\n"))
	(let ((start (point)))
	  (xgtags-insert-with-face search 'xgtags-match-face)
	  (insert "[")
	  (xgtags-insert-with-face (number-to-string line)
				  'xgtags-line-number-face)
	  (when match
	    (insert "]\t\t")
	    (xgtags-insert-with-face match 'xgtags-line-face))
	  (put-text-property start (point) 'xgtags-tag tag))
	(insert "\n")))))


;;;###autoload
(define-minor-mode xgtags-mode
  "Toggle xgtags-mode, a minor mode for browsing source code using GLOBAL.

Input tag name and move to the definition.
	\\[xgtags-find-tag]
Input tag name and move to the referenced point.
	\\[xgtags-find-rtag]
Input symbol and move to the locations.
	\\[xgtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[xgtags-find-with-grep]
Input pattern, search with id-utils(1) and move to the locations.
	\\[xgtags-find-with-idutils]
Input pattern and move to the top of the file.
	\\[xgtags-find-file]
Get the expression as a tagname around here and move there.
	\\[xgtags-find-tag-from-here]
Display current screen on hypertext browser.
	\\[xgtags-display-browser]
Get the expression as a tagname around here and move there.
	\\[xgtags-find-tag-by-event]
Move to previous point on the stack.
	\\[xgtags-pop-stack]
Make tag name list for completion.
	\\[xgtags-make-complete-list]

Key definitions:
\\{xgtags-mode-map}
Turning on xgtags-mode calls the value of the variable `xgtags-mode-hook'
with no args, if that value is non-nil."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  xgtags-minor-mode-text
  ;; The minor mode bindings.
  'xgtags-mode-map)


(define-derived-mode xgtags-select-mode fundamental-mode xgtags-select-mode-name
  "Major mode for choosing a tag from tags list.

Select a tag in tags list and move there.
	\\[xgtags-select-tag]
Move to previous point on the stack.
	\\[xgtags-pop-stack]

Key definitions:
\\{xgtags-select-mode-map}
Turning on xgtags-select mode calls the value of the variable
`xgtags-select-mode-hook' with no args, if that value is non-nil."
  (interactive)
  (setq buffer-read-only t
	truncate-lines t)
  (goto-char (point-min))
  (make-local-variable 'xgtags-select-tags))


(provide 'xgtags)

;;; xgtags.el ends here
