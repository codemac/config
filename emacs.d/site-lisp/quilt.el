;;; quilt.el v0.4 - a minor mode for working with files in quilt
;;; http://selenic.com/quilt/
;;;
;;; Copyright 2005  Matt Mackall <mpm@selenic.com>
;;;
;;; This software may be used and distributed according to the terms
;;; of the GNU General Public License, incorporated herein by reference.
;;;
;;; Usage: add (load "~/quilt.el") to your .emacs file

(defun quilt-find-dir (fn)
  "find the top level dir for quilt from fn"
  (let* ((d (file-name-directory fn)))
    (if (or (not fn) (equal fn "/"))
	nil
      (if (file-accessible-directory-p (concat d "/.pc"))
	  d
	(quilt-find-dir (directory-file-name d))))))

(defun quilt-dir (&optional fn)
  (quilt-find-dir (if fn fn buffer-file-name)))

(defun quilt-drop-dir (fn)
  (let* ((d (quilt-find-dir fn)))
    (substring fn (length d) (length fn))))

(defun quilt-p (&optional fn)
  "check if the given file or current buffer is in a quilt tree"
  (if (quilt-dir fn) 't nil))

(defun quilt-save ()
  (save-some-buffers nil 'quilt-p))

(defun quilt-owned-p (fn)
  "check if the current buffer is quilt controlled"
  (if (not fn)
      nil
    (let* ((pd (file-name-nondirectory 
		(directory-file-name (file-name-directory fn)))))
      (and 
       (not (string-match "\\(~$\\|\\.rej$\\)" fn))
       (not (equal pd "patches"))
       (not (equal pd ".pc"))
       (quilt-p fn)))))

(defun quilt-cmd (cmd &optional buf)
  "execute a quilt command at the top of the quilt tree for the given buffer"
  (let* ((d default-directory))
    (cd (quilt-dir))
    (shell-command (concat "quilt " cmd) buf)
    (cd d)))

(defun quilt-cmd-to-string (cmd)
  "execute a quilt command at the top of the quilt tree for the given buffer"
  (let* ((d default-directory))
    (cd (quilt-dir))
    (let ((r (shell-command-to-string (concat "quilt " cmd))))
      (cd d) r)))

(defun quilt-applied-list ()
  (split-string (quilt-cmd-to-string "applied") "\n"))

(defun quilt-file-list ()
  (split-string (quilt-cmd-to-string "files") "\n"))

(defun quilt-patch-list ()
  (split-string (quilt-cmd-to-string "series") "\n"))

(defun quilt-top-patch ()
  (let* ((top (quilt-cmd-to-string "top")))
    (if (equal top "")
	nil
	(substring top 0 -1))))

(defun quilt-complete-list (p l)
  (defun to-alist (list n)
    (if list
	(cons (cons (car list) n)
	      (to-alist (cdr list) (+ 1 n)))
      nil))
  (completing-read p (to-alist l 0) nil t))

(defun quilt-editable (f)
  (let* ((qd (quilt-dir))
	 (fn (quilt-drop-dir f)))
    (defun editable (file dirs)
      (if (car dirs)
	  (if (file-exists-p (concat qd ".pc/" (car dirs) "/" file))
	      't
	    (editable file (cdr dirs)))
	nil))
    (editable fn (if quilt-edit-top-only
		     (list (quilt-top-patch))
		     (cdr (cdr (directory-files (concat qd ".pc/"))))))))

(defun quilt-short-patchname ()
  (let* ((p (quilt-top-patch)))
    (if (not p)
	"none"
      (let* ((p2 (file-name-sans-extension p)))
	   (if (< (length p2) 10)
	       p2
	     (concat (substring p2 0 8) ".."))))))

(defun quilt-update-modeline ()
  (interactive)
  (defvar quilt-mode-line nil)
  (make-variable-buffer-local 'quilt-mode-line)
  (setq quilt-mode-line 
	(concat " Q:" (quilt-short-patchname)))
  (force-mode-line-update))

(defun quilt-revert ()
  (defun revert (buf)
    (save-excursion
      (set-buffer buf)
      (if (quilt-owned-p buffer-file-name)
	  (quilt-update-modeline))
      (if (and (quilt-owned-p buffer-file-name)
	       (not (buffer-modified-p)))
	  (revert-buffer 't 't))))
  (defun revert-list (buffers)
    (if (not (cdr buffers))
	nil
      (revert (car buffers))
      (revert-list (cdr buffers))))
  (revert-list (buffer-list)))

(defun quilt-push (arg)
  "Push next patch, force with prefix arg"
  (interactive "p")
  (quilt-save)
  (if (> arg 1)
      (quilt-cmd "push -f" "*quilt*")
    (quilt-cmd "push -q"))
  (quilt-revert))

(defun quilt-pop (arg)
  "Pop top patch, force with prefix arg"
  (interactive "p")
  (quilt-save)
  (if (> arg 1)
      (quilt-cmd "pop -f")
    (quilt-cmd "pop -q"))
  (quilt-revert))

(defun quilt-push-all (arg)
  "Push all remaining patches"
  (interactive "p")
  (quilt-save)
  (if (> arg 1)
      (quilt-cmd "push -f" "*quilt*")
    (quilt-cmd "push -qa"))
  (quilt-revert))

(defun quilt-pop-all (arg)
  "Pop all applied patches, force with prefix arg"
  (interactive "p")
  (quilt-save)
  (if (> arg 1)
      (quilt-cmd "pop -af")
    (quilt-cmd "pop -qa"))
  (quilt-revert))

(defun quilt-goto ()
  "Go to a specified patch"
  (interactive)
  (let* ((arg (quilt-complete-list "Goto patch: " (quilt-patch-list))))
       (quilt-save)
       (if (file-exists-p (concat (quilt-dir) ".pc/" arg))
	   (quilt-cmd (concat "pop -q " arg) "*quilt*")
	 (quilt-cmd (concat "push -q " arg) "*quilt*")))
  (quilt-revert))

(defun quilt-top ()
  "Display topmost patch"
  (interactive)
  (quilt-cmd "top"))

(defun quilt-find-file ()
  "Find a file in the topmost patch"
  (interactive)
  (find-file (concat (quilt-dir)
		     (quilt-complete-list "File: " (quilt-file-list)))))

(defun quilt-files ()
  "Display files in topmost patch"
  (interactive)
  (quilt-cmd "files"))

(defun quilt-import (fn pn)
  "Display files in topmost patch"
  (interactive "fPatch to import: \nsPatch name: ")
  (quilt-cmd (concat "import -n " pn ".patch " fn)))

(defun quilt-diff ()
  "Display diff of current changes"
  (interactive)
  (quilt-save)
  (quilt-cmd "diff" "*diff*"))

(defun quilt-new (f)
  "Create a new patch"
  (interactive "sPatch name:")
  (quilt-save)
  (quilt-cmd (concat "new " f ".patch"))
  (quilt-revert))

(defun quilt-applied ()
  "Show applied patches"
  (interactive)
  (quilt-cmd "applied" "*quilt*"))

(defun quilt-add (arg)
  "Add a file to the current patch"
  (interactive "@b")
  (quilt-cmd (concat "add " (quilt-drop-dir buffer-file-name)))
  (quilt-revert))

(defun quilt-edit-patch ()
  "Edit the topmost patch"
  (interactive)
  (quilt-save)
  (find-file (concat (quilt-dir) "/patches/" (quilt-top-patch))))

(defun quilt-patches ()
  "Show which patches modify the current buffer"
  (interactive)
  (quilt-cmd (concat "patches " (quilt-drop-dir buffer-file-name))))

(defun quilt-unapplied ()
  "Display unapplied patch list"
  (interactive)
  (quilt-cmd "unapplied" "*quilt*"))

(defun quilt-refresh ()
  "Refresh the current patch"
  (interactive)
  (quilt-save)
  (quilt-cmd "refresh"))

(defun quilt-remove ()
  "Remove a file from the current patch and revert it"
  (interactive)
  (let* ((f (quilt-drop-dir buffer-file-name)))
    (if (y-or-n-p (format "Really drop %s? " f))
	(quilt-cmd (concat "remove " f))))
  (quilt-revert))

(defun quilt-edit-series ()
  "Edit the patch series file"
  (interactive)
  (find-file (concat (quilt-find-dir buffer-file-name) "/patches/series")))

(defvar quilt-mode-map (make-sparse-keymap))
(define-key quilt-mode-map "\C-cqt" 'quilt-top)
(define-key quilt-mode-map "\C-cqf" 'quilt-find-file)
(define-key quilt-mode-map "\C-cqF" 'quilt-files)
(define-key quilt-mode-map "\C-cqd" 'quilt-diff)
(define-key quilt-mode-map "\C-cqp" 'quilt-push)
(define-key quilt-mode-map "\C-cqo" 'quilt-pop)
(define-key quilt-mode-map "\C-cqP" 'quilt-push-all)
(define-key quilt-mode-map "\C-cqO" 'quilt-pop-all)
(define-key quilt-mode-map "\C-cqg" 'quilt-goto)
(define-key quilt-mode-map "\C-cqA" 'quilt-applied)
(define-key quilt-mode-map "\C-cqn" 'quilt-new)
(define-key quilt-mode-map "\C-cqd" 'quilt-diff)
(define-key quilt-mode-map "\C-cqi" 'quilt-import)
(define-key quilt-mode-map "\C-cqa" 'quilt-add)
(define-key quilt-mode-map "\C-cqe" 'quilt-edit-patch)
(define-key quilt-mode-map "\C-cqm" 'quilt-patches)
(define-key quilt-mode-map "\C-cqu" 'quilt-unapplied)
(define-key quilt-mode-map "\C-cqr" 'quilt-refresh)
(define-key quilt-mode-map "\C-cqR" 'quilt-remove)
(define-key quilt-mode-map "\C-cqs" 'quilt-edit-series)

(defvar quilt-mode nil)
(make-variable-buffer-local 'quilt-mode)
(defvar quilt-edit-top-only 't)

(defun quilt-mode (&optional arg)
  "Toggle quilt-mode. With positive arg, enable quilt-mode.

\\{quilt-mode-map}
"
  (interactive "p")
  (setq quilt-mode 
	(if (null arg) 
	    (not quilt-mode) 
	  (> (prefix-numeric-value arg) 0)))
  (if quilt-mode
      (let* ((f buffer-file-name))
	(if (quilt-owned-p f)
	    (if (not (quilt-editable f))
		(toggle-read-only 1)
	      (toggle-read-only 0)))
	(quilt-update-modeline))))

(defun quilt-hook ()
  "Enable quilt mode for quilt-controlled files."
  (if (quilt-p) (quilt-mode 1)))

(add-hook 'find-file-hooks 'quilt-hook)
(add-hook 'after-revert-hook 'quilt-hook)

(or (assq 'quilt-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(quilt-mode quilt-mode-line) minor-mode-alist)))  

(or (assq 'quilt-mode-map minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'quilt-mode quilt-mode-map) minor-mode-map-alist)))
