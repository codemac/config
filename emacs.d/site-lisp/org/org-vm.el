;;; org-vm.el --- Support for links to VM messages from within Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; This file implements links to VM messages and folders from within Org-mode.
;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org)

;; Declare external functions and variables
(declare-function vm-beginning-of-message "ext:vm-page" ())
(declare-function vm-follow-summary-cursor "ext:vm-motion" ())
(declare-function vm-get-header-contents "ext:vm-summary"
		  (message header-name-regexp &optional clump-sep))
(declare-function vm-isearch-narrow "ext:vm-search" ())
(declare-function vm-isearch-update "ext:vm-search" ())
(declare-function vm-select-folder-buffer "ext:vm-macro" ())
(declare-function vm-su-message-id "ext:vm-summary" (m))
(declare-function vm-su-subject "ext:vm-summary" (m))
(declare-function vm-summarize "ext:vm-summary" (&optional display raise))
(defvar vm-message-pointer)
(defvar vm-folder-directory)

;; Install the link type
(org-add-link-type "vm" 'org-vm-open)
(add-hook 'org-store-link-functions 'org-vm-store-link)

;; Implementation
(defun org-vm-store-link ()
  "Store a link to a VM folder or message."
  (when (or (eq major-mode 'vm-summary-mode)
	    (eq major-mode 'vm-presentation-mode))
    (and (eq major-mode 'vm-presentation-mode) (vm-summarize))
    (vm-follow-summary-cursor)
    (save-excursion
      (vm-select-folder-buffer)
      (let* ((message (car vm-message-pointer))
	     (folder buffer-file-name)
	     (subject (vm-su-subject message))
	     (to (vm-get-header-contents message "To"))
	     (from (vm-get-header-contents message "From"))
	     (message-id (vm-su-message-id message))
	     desc link)
	(org-store-link-props :type "vm" :from from :to to :subject subject
			      :message-id message-id)
	(setq message-id (org-remove-angle-brackets message-id))
	(setq folder (abbreviate-file-name folder))
	(if (string-match (concat "^" (regexp-quote vm-folder-directory))
			  folder)
	    (setq folder (replace-match "" t t folder)))
	(setq desc (org-email-link-description))
	(setq link (org-make-link "vm:" folder "#" message-id))
	(org-add-link-props :link link :description desc)
	link))))

(defun org-vm-open (path)
  "Follow a VM message link specified by PATH."
  (let (folder article)
    (if (not (string-match "\\`\\([^#]+\\)\\(#\\(.*\\)\\)?" path))
	(error "Error in VM link"))
    (setq folder (match-string 1 path)
	  article (match-string 3 path))
    ;; The prefix argument will be interpreted as read-only
    (org-vm-follow-link folder article current-prefix-arg)))

(defun org-vm-follow-link (&optional folder article readonly)
  "Follow a VM link to FOLDER and ARTICLE."
  (require 'vm)
  (setq article (org-add-angle-brackets article))
  (if (string-match "^//\\([a-zA-Z]+@\\)?\\([^:]+\\):\\(.*\\)" folder)
      ;; ange-ftp or efs or tramp access
      (let ((user (or (match-string 1 folder) (user-login-name)))
	    (host (match-string 2 folder))
	    (file (match-string 3 folder)))
	(cond
	 ((featurep 'tramp)
	  ;; use tramp to access the file
	  (if (featurep 'xemacs)
	      (setq folder (format "[%s@%s]%s" user host file))
	    (setq folder (format "/%s@%s:%s" user host file))))
	 (t
	  ;; use ange-ftp or efs
	  (require (if (featurep 'xemacs) 'efs 'ange-ftp))
	  (setq folder (format "/%s@%s:%s" user host file))))))
  (when folder
    (funcall (cdr (assq 'vm org-link-frame-setup)) folder readonly)
    (sit-for 0.1)
    (when article
      (vm-select-folder-buffer)
      (widen)
      (let ((case-fold-search t))
	(goto-char (point-min))
	(if (not (re-search-forward
		  (concat "^" "message-id: *" (regexp-quote article))))
	    (error "Could not find the specified message in this folder"))
	(vm-isearch-update)
	(vm-isearch-narrow)
	(vm-beginning-of-message)
	(vm-summarize)))))

(provide 'org-vm)

;;; org-vm.el ends here
