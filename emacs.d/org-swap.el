(require 'desktop)

(defun org-swap-save (refiletarget &optional include-frames)
  (interactive)
  ;; check if what we're about to save is *newer* than what we're saving
  
  
  
  )

(defun desktop-save (dirname &optional release only-if-changed)
  (interactive (list
                ;; Or should we just use (car desktop-path)?
                (let ((default (if (member "." desktop-path)
                                   default-directory
                                 user-emacs-directory)))
                  (read-directory-name "Directory to save desktop file in: "
                                       default default t))))
  (setq desktop-dirname (file-name-as-directory (expand-file-name dirname)))
  (save-excursion
    (let ((eager desktop-restore-eager)
	  (new-modtime (nth 5 (file-attributes (desktop-full-file-name)))))
      (when
	  (or (not new-modtime)		; nothing to overwrite
	      (equal desktop-file-modtime new-modtime)
	      (yes-or-no-p (if desktop-file-modtime
			       (if (> (float-time new-modtime) (float-time desktop-file-modtime))
				   "Desktop file is more recent than the one loaded.  Save anyway? "
				 "Desktop file isn't the one loaded.  Overwrite it? ")
			     "Current desktop was not loaded from a file.  Overwrite this desktop file? "))
	      (unless release (error "Desktop file conflict")))

	;; If we're done with it, release the lock.
	;; Otherwise, claim it if it's unclaimed or if we created it.
	(if release
	    (desktop-release-lock)
	  (unless (and new-modtime (desktop-owner)) (desktop-claim-lock)))

	(with-temp-buffer
	  (insert
	   ";; -*- mode: emacs-lisp; coding: emacs-mule; -*-\n"
	   desktop-header
	   ";; Created " (current-time-string) "\n"
	   ";; Desktop file format version " desktop-file-version "\n"
	   ";; Emacs version " emacs-version "\n")
	  (save-excursion (run-hooks 'desktop-save-hook))
	  (goto-char (point-max))
	  (insert "\n;; Global section:\n")
	  ;; Called here because we save the window/frame state as a global
	  ;; variable for compatibility with previous Emacsen.
	  (desktop-save-frameset)
	  (unless (memq 'desktop-saved-frameset desktop-globals-to-save)
	    (desktop-outvar 'desktop-saved-frameset))
	  (mapc (function desktop-outvar) desktop-globals-to-save)
	  (setq desktop-saved-frameset nil) ; after saving desktop-globals-to-save
	  (when (memq 'kill-ring desktop-globals-to-save)
	    (insert
	     "(setq kill-ring-yank-pointer (nthcdr "
	     (int-to-string (- (length kill-ring) (length kill-ring-yank-pointer)))
	     " kill-ring))\n"))

	  (insert "\n;; Buffer section -- buffers listed in same order as in buffer list:\n")
	  (dolist (l (mapcar 'desktop-buffer-info (buffer-list)))
	    (let ((base (pop l)))
	      (when (apply 'desktop-save-buffer-p l)
		(insert "("
			(if (or (not (integerp eager))
				(if (zerop eager)
				    nil
				  (setq eager (1- eager))))
			    "desktop-create-buffer"
			  "desktop-append-buffer-args")
			" "
			desktop-file-version)
		;; If there's a non-empty base name, we save it instead of the buffer name
		(when (and base (not (string= base "")))
		  (setcar (nthcdr 1 l) base))
		(dolist (e l)
		  (insert "\n  " (desktop-value-to-string e)))
		(insert ")\n\n"))))

	  (setq default-directory desktop-dirname)
	  ;; When auto-saving, avoid writing if nothing has changed since the last write.
	  (let* ((beg (and only-if-changed
			   (save-excursion
			     (goto-char (point-min))
			     ;; Don't check the header with changing timestamp
			     (and (search-forward "Global section" nil t)
				  ;; Also skip the timestamp in desktop-saved-frameset
				  ;; if it's saved in the first non-header line
				  (search-forward "desktop-saved-frameset"
						  (line-beginning-position 3) t)
				  ;; This is saved after the timestamp
				  (search-forward (format "%S" desktop--app-id) nil t))
			     (point))))
		 (checksum (and beg (md5 (current-buffer) beg (point-max) 'emacs-mule))))
	    (unless (and checksum (equal checksum desktop-file-checksum))
	      (let ((coding-system-for-write 'emacs-mule))
		(write-region (point-min) (point-max) (desktop-full-file-name) nil 'nomessage))
	      (setq desktop-file-checksum checksum)
	      ;; We remember when it was modified (which is presumably just now).
	      (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name)))))))))))


(provide 'org-swap)
