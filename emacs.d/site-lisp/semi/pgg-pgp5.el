;;; pgg-pgp5.el --- PGP 5.* support for PGG.

;; Copyright (C) 1999,2000 Daiki Ueno

;; Author: Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Created: 1999/11/02
;; Keywords: PGP, OpenPGP

;; This file is part of SEMI (Secure Emacs MIME Interface).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'mel) ; binary-to-text-funcall, binary-write-decoded-region
(eval-when-compile (require 'pgg))

(defgroup pgg-pgp5 ()
  "PGP 5.* interface"
  :group 'pgg)

(defcustom pgg-pgp5-pgpe-program "pgpe"
  "PGP 5.* 'pgpe' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-pgps-program "pgps"
  "PGP 5.* 'pgps' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-pgpk-program "pgpk"
  "PGP 5.* 'pgpk' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-pgpv-program "pgpv"
  "PGP 5.* 'pgpv' executable."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-shell-file-name "/bin/sh"
  "File name to load inferior shells from.
Bourne shell or its equivalent \(not tcsh) is needed for \"2>\"."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-shell-command-switch "-c"
  "Switch used to have the shell execute its command line argument."
  :group 'pgg-pgp5
  :type 'string)

(defcustom pgg-pgp5-extra-args nil
  "Extra arguments for every PGP 5.* invocation."
  :group 'pgg-pgp5
  :type 'string)

(eval-and-compile
  (luna-define-class pgg-scheme-pgp5 (pgg-scheme)))

(defvar pgg-pgp5-user-id nil
  "PGP 5.* ID of your default identity.")

(defvar pgg-scheme-pgp5-instance nil)

;;;###autoload
(defun pgg-make-scheme-pgp5 ()
  (or pgg-scheme-pgp5-instance
      (setq pgg-scheme-pgp5-instance
	    (luna-make-entity 'pgg-scheme-pgp5))))

(defun pgg-pgp5-process-region (start end passphrase program args)
  (let* ((errors-file-name (make-temp-file "pgg-errors"))
	 (args
	  (append args
		  pgg-pgp5-extra-args
		  (list (concat "2>" errors-file-name))))
	 (shell-file-name pgg-pgp5-shell-file-name)
	 (shell-command-switch pgg-pgp5-shell-command-switch)
	 (process-environment process-environment)
	 (output-buffer pgg-output-buffer)
	 (errors-buffer pgg-errors-buffer)
	 (process-connection-type nil)
	 process status exit-status)
    (with-current-buffer (get-buffer-create output-buffer)
      (buffer-disable-undo)
      (erase-buffer))
    (when passphrase
      (setenv "PGPPASSFD" "0"))
    (unwind-protect
	(progn
	  (setq process
		(apply #'binary-funcall
		       #'start-process-shell-command "*PGP*" output-buffer
		       program args))
	  (set-process-sentinel process #'ignore)
	  (when passphrase
	    (process-send-string process (concat passphrase "\n")))
	  (process-send-region process start end)
	  (process-send-eof process)
	  (while (eq 'run (process-status process))
	    (accept-process-output process 5))
	  (setq status (process-status process)
		exit-status (process-exit-status process))
	  (delete-process process)
	  (with-current-buffer output-buffer
	    (pgg-convert-lbt-region (point-min)(point-max) 'LF)

	    (if (memq status '(stop signal))
		(error "%s exited abnormally: '%s'" program exit-status))
	    (if (= 127 exit-status)
		(error "%s could not be found" program))

	    (set-buffer (get-buffer-create errors-buffer))
	    (buffer-disable-undo)
	    (erase-buffer)
	    (insert-file-contents errors-file-name)))
      (if (and process (eq 'run (process-status process)))
	  (interrupt-process process))
      (condition-case nil
	  (delete-file errors-file-name)
	(file-error nil)))))

(luna-define-method pgg-scheme-lookup-key ((scheme pgg-scheme-pgp5)
						  string &optional type)
  (let ((args (list "+language=en" "-l" string)))
    (with-current-buffer (get-buffer-create pgg-output-buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (apply #'call-process pgg-pgp5-pgpk-program nil t nil args)
      (goto-char (point-min))
      (when (re-search-forward "^sec" nil t)
	(substring
	 (nth 2 (split-string
		 (buffer-substring (match-end 0)(progn (end-of-line)(point)))))
	 2)))))

(luna-define-method pgg-scheme-encrypt-region ((scheme pgg-scheme-pgp5)
					       start end recipients)
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (args
	  `("+NoBatchInvalidKeys=off" "-fat" "+batchmode=1"
	    ,@(if recipients
		  (apply #'append
			 (mapcar (lambda (rcpt)
				   (list "-r"
					 (concat "\"" rcpt "\"")))
				 (append recipients
					 (if pgg-encrypt-for-me
					     (list pgg-pgp5-user-id)))))))))
    (pgg-pgp5-process-region start end nil pgg-pgp5-pgpe-program args)
    (pgg-process-when-success nil)))

(luna-define-method pgg-scheme-decrypt-region ((scheme pgg-scheme-pgp5)
					       start end)
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (passphrase
	  (pgg-read-passphrase
	   (format "PGP passphrase for %s: " pgg-pgp5-user-id)
	   (pgg-scheme-lookup-key scheme pgg-pgp5-user-id 'encrypt)))
	 (args
	  '("+verbose=1" "+batchmode=1" "+language=us" "-f")))
    (pgg-pgp5-process-region start end passphrase pgg-pgp5-pgpv-program args)
    (pgg-process-when-success nil)))

(luna-define-method pgg-scheme-sign-region ((scheme pgg-scheme-pgp5)
					    start end &optional clearsign)
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (passphrase
	  (pgg-read-passphrase
	   (format "PGP passphrase for %s: " pgg-pgp5-user-id)
	   (pgg-scheme-lookup-key scheme pgg-pgp5-user-id 'sign)))
	 (args
	  (list (if clearsign "-fat" "-fbat")
		"+verbose=1" "+language=us" "+batchmode=1"
		"-u" pgg-pgp5-user-id)))
    (pgg-pgp5-process-region start end passphrase pgg-pgp5-pgps-program args)
    (pgg-process-when-success
      (when (re-search-forward "^-+BEGIN PGP SIGNATURE" nil t);XXX
	(let ((packet
	       (cdr (assq 2 (pgg-parse-armor-region
			     (progn (beginning-of-line 2)
				    (point))
			     (point-max))))))
	  (if pgg-cache-passphrase
	      (pgg-add-passphrase-cache
	       (cdr (assq 'key-identifier packet))
	       passphrase)))))))

(luna-define-method pgg-scheme-verify-region ((scheme pgg-scheme-pgp5)
					      start end &optional signature)
  (let ((orig-file (make-temp-file "pgg"))
	(args '("+verbose=1" "+batchmode=1" "+language=us"))
	(orig-mode (default-file-modes)))
    (unwind-protect
	(progn
	  (set-default-file-modes 448)
	  (binary-write-decoded-region start end orig-file))
      (set-default-file-modes orig-mode))
    (when (stringp signature)
      (copy-file signature (setq signature (concat orig-file ".asc")))
      (setq args (append args (list signature))))
    (pgg-pgp5-process-region (point)(point) nil pgg-pgp5-pgpv-program args)
    (delete-file orig-file)
    (if signature (delete-file signature))
    (with-current-buffer pgg-errors-buffer
      (goto-char (point-min))
      (if (re-search-forward "^Good signature" nil t)
	  (progn
	    (set-buffer pgg-output-buffer)
	    (insert-buffer-substring pgg-errors-buffer)
	    t)
	nil))))

(luna-define-method pgg-scheme-insert-key ((scheme pgg-scheme-pgp5))
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (args
	  (list "+verbose=1" "+batchmode=1" "+language=us" "-x"
		(concat "\"" pgg-pgp5-user-id "\""))))
    (pgg-pgp5-process-region (point)(point) nil pgg-pgp5-pgpk-program args)
    (insert-buffer-substring pgg-output-buffer)))

(luna-define-method pgg-scheme-snarf-keys-region ((scheme pgg-scheme-pgp5)
						  start end)
  (let* ((pgg-pgp5-user-id (or pgg-pgp5-user-id pgg-default-user-id))
	 (key-file (make-temp-file "pgg"))
	 (args
	  (list "+verbose=1" "+batchmode=1" "+language=us" "-a"
		key-file)))
    (let ((coding-system-for-write 'raw-text-dos))
      (write-region start end key-file))
    (pgg-pgp5-process-region start end nil pgg-pgp5-pgpk-program args)
    (delete-file key-file)
    (pgg-process-when-success nil)))

(provide 'pgg-pgp5)

;;; pgg-pgp5.el ends here
