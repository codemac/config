;;; ob-scheme.el --- org-babel functions for Scheme

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research, scheme
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Now working with SBCL for both session and external evaluation.
;;
;; This certainly isn't optimally robust, but it seems to be working
;; for the basic use cases.

;;; Requirements:

;; - a working scheme implementation
;;   (e.g. guile http://www.gnu.org/software/guile/guile.html)
;;
;; - for session based evaluation cmuscheme.el is required which is
;;   included in Emacs

;;; Code:
(require 'ob)
(load-library "geiser-impl")

(defvar org-babel-default-header-args:scheme '()
  "Default header arguments for scheme code blocks.")

(defcustom org-babel-scheme-default-impl nil
  "Name of scheme implementation to use to evaluate scheme blocks.
   (must be supported by geiser)."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defun org-babel-expand-body:scheme (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (if (> (length vars) 0)
        (concat "(let ("
                (mapconcat
                 (lambda (var) (format "%S" (print `(,(car var) ',(cdr var)))))
                 vars "\n      ")
                ")\n" body ")")
      body)))


;; Map session names to buffers
(setq org-babel-scheme-repl-map (make-hash-table :test 'equal))

;; Remove dead buffers from repl map
(defun cleanse-org-babel-scheme-repl-map ()
  (maphash
   (lambda (x y)
     (when (not (buffer-name y))
       (remhash x org-babel-scheme-repl-map)))
   org-babel-scheme-repl-map))

(defun org-babel-scheme-get-session-buffer (session-name)
  (cleanse-org-babel-scheme-repl-map)
  (gethash session-name org-babel-scheme-repl-map))

(defun org-babel-scheme-set-session-buffer (session-name buffer)
  (puthash session-name buffer org-babel-scheme-repl-map))

(defun org-babel-scheme-get-buffer-impl (buffer)
  (with-current-buffer (set-buffer buffer)
    geiser-impl--implementation))

;; Switch to REPL, creating it if it doesn't exist:
(defun org-babel-scheme-get-repl (impl name)
  (let ((buffer (org-babel-scheme-get-session-buffer name)))
    (or buffer 
    (progn
      (run-geiser impl)
      (if name
	  (progn 
	    (rename-buffer name t)
	    (org-babel-scheme-set-session-buffer name (current-buffer)))
	)
      (current-buffer)
  ))))

;; Generate a name for the session buffer.
;;
;; For a named session, the buffer name will be the session name.
;;
;; If the session is unnamed (nil), generate a name.
;;
;; If the session is "none", use nil for the session name, and
;; org-babel-scheme-execute-with-geiser will use a temporary session.
;;
;; Otherwise, create temporary 
(defun org-babel-scheme-make-session-name (buffer name impl)
  (message "org-babel-scheme-make-session-name -%s-%s-%s"
	   buffer name impl)
  (let ((result 
	 (cond ((not name)
		(concat buffer " " (symbol-name impl) " REPL"))
	       ((string= name "none") nil)
	       (name))))
    (message " --> %s" result)
    result))

(defun org-babel-scheme-execute-with-geiser (code output impl repl)
  (let ((result nil))
    (with-temp-buffer
      (insert (format ";; -*- geiser-scheme-implementation: %s -*-" impl))
      (newline)
      (insert (if output
		  (format "(with-output-to-string (lambda () %s))" code)
		code)
	      )
      (geiser-mode)
      (let ((repl-buffer (save-current-buffer (org-babel-scheme-get-repl impl repl))))
	(when (not (eq impl (org-babel-scheme-get-buffer-impl (current-buffer))))
	  (message "Implementation mismatch: %s (%s) %s (s)" impl (symbolp impl)
		   (org-babel-scheme-get-buffer-impl (current-buffer))
		   (symbolp (org-babel-scheme-get-buffer-impl (current-buffer)))))
	(setq geiser-repl--repl repl-buffer)
	(setq geiser-impl--implementation nil)
	(geiser-eval-region (point-min) (point-max))
	(setq result (substring (current-message) 3))
	(when (not repl)
	  (save-current-buffer (set-buffer repl-buffer)
			       (geiser-repl-exit))
	  (set-process-query-on-exit-flag (get-buffer-process repl-buffer) nil)
	  (kill-buffer repl-buffer))
	(setq result (if (or (string= result "#<void>")
			     (string= result "#<unspecified>"))
			 nil
		       (read result)))))
    result
    ))

(defun org-babel-execute:scheme (body params)
  "Execute a block of Scheme code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (let* ((source-buffer (current-buffer))
	 (source-buffer-name (replace-regexp-in-string ;; zap surrounding *
	  "^ ?\\*\\([^*]+\\)\\*" "\\1" (buffer-name source-buffer))))
    (save-excursion
      (org-babel-reassemble-table
       (let* ((result-type (cdr (assoc :result-type params)))
	      (impl (or (when (cdr (assoc :scheme params))
			  (intern (cdr (assoc :scheme params))))
			org-babel-scheme-default-impl
			geiser-default-implementation
			(car geiser-active-implementations)))
	      (session (org-babel-scheme-make-session-name source-buffer-name (cdr (assoc :session params)) impl))
	    (full-body (org-babel-expand-body:scheme body params)))
       (org-babel-scheme-execute-with-geiser full-body           ; code
					     (string= result-type "output")	; output?
					     impl ; implementation
					     (and (not (string= session "none")) session)); session
       )
     (org-babel-pick-name (cdr (assoc :colname-names params))
			  (cdr (assoc :colnames params)))
    (org-babel-pick-name (cdr (assoc :rowname-names params))
			 (cdr (assoc :rownames params)))))))
  

(provide 'ob-scheme)

;;; ob-scheme.el ends here
