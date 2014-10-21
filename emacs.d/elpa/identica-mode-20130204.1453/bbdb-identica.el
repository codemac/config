;;; bbdb-identica.el ---
;;
;; Filename: bbdb-identica.el
;; Description:
;; Author: Christian
;; Maintainer:
;; Created: dom oct  2 22:15:13 2011 (-0300)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   BBDB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This should work in BBDB V.3.x for now...
;; It is in heavy, really heavy, development.
;;
;; As far I tried, I couldn't make it work in the way proposed by BBDB. 
;; I couldn't find any documentation of how to use the MUA API.
;; For now, I will use every possible command despite it is not desirable
;; for BBDB developers(I think :-S ).
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'identica-mode)
(require 'identica-friends)

                                        ; Identica-friends-buffer
;; Identica friends buffer must have a way to introduce people into BBDB.
;; There's need of creating a new field into a record. This field will be called "identica".

;; We'll define a ':' key for introducing a new record into BBDB or updating a record.

(defcustom bbdb/identica-update-records-p
  (lambda ()
    (let ((bbdb-update-records-p 'query ))
      (bbdb-select-message)))
  "How `bbdb-mua-update-records' processes mail addresses in Identica and Identica-friends.
Allowed values are:
 nil          Do nothing.
 search       Search for existing records.
 query        Update existing records or query for creating new ones.
 create or t  Update existing records or create new ones.
A function which returns one of the above values."
  :group 'bbdb-mua-identica
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records"
                        (lambda () (let ((bbdb-update-records-p 'search))
                                     (bbdb-select-message))))
                 (const :tag "query annotation of all messages"
                        (lambda () (let ((bbdb-update-records-p 'query))
                                     (bbdb-select-message))))
                 ;; (const :tag "annotate (query) only new messages"
                 ;;        (lambda ()
                 ;;          (let ((bbdb-update-records-p
                 ;;                 (if (bbdb/rmail-new-flag) 'query 'search)))
                 ;;            (bbdb-select-message))))
                 (const :tag "annotate all messages"
                        (lambda () (let ((bbdb-update-records-p 'create))
                                     (bbdb-select-message))))
                 (const :tag "accept messages" bbdb-accept-message)
                 (const :tag "ignore messages" bbdb-ignore-message)
                 (const :tag "select messages" bbdb-select-message)
                 (sexp  :tag "user defined function")))

;; (defun bbdb/identica-header (header)
;;   ""
;; )
                                        ; --------------------
                                        ; Insinuation
                                        ; --------------------

;; It doesn't work :-( Is still under development.
;; 
;; ;;;###autoload
;; (defun bbdb-insinuate-identica ()
;;   "Add every keymap and hooks necesary for using BBDB into `identica-friends-mode'.
;; You shouldn't call this in your init file, instead use `bbdb-initialize'"
;;   (define-key identica-friends-mode-map ":" 'bbdb-mua-display-sender)
;;   (define-key identica-friends-mode-map ";" 'bbdb-mua-edit-notes-sender)
;; )



;; ;; We have to make bbdb-mua recognice identica-friends-mode, if not it will fall-back with error.
;; (defadvice bbdb-mua (before identica-bbdb-mua ())
;;   "This advice add into `bbdb-mua' the necessary for BBDB to recognice identica-friends-mode, and identica-mode."
;;   (if (member major-mode '(identica-mode identica-friends-mode))
;;       'identica)
;;   )

;; Activate identica-bbdb-mua advice
;; (ad-activate 'bbdb-mua)
;; (ad-deactivate 'bbdb-mua)


					; ____________________

(defun bbdb-identica-friends-next-usr ()
  "This function is supposed to be used as a hook to the function `identica-friends-next-user'.
Check if the actual user is in BBDB. If not, add it *without query the user*. 

Remember: 
Adding into the BBDB means: 
1) to create a new BBDB record with the same name of the identica user name(NOT NICK!)
2) Add the nick into a new field called \"identica\"."
  (setq usr (identica-friends-get-current-user))
  ;; Our idea is to show the user if founded...
  ;; Search for the *first mach* in the BBDB:
  (setq record 
	(let ((usr-name (nth 1 usr)))
	  (car (bbdb-search (bbdb-records) usr-name))
	  )
	)
 
  ;; check if exist, if not add it(or query to add it).
  (if record 
      (progn 
	(bbdb-display-records (cons record '()))
	(unless (bbdb-identica-check-record record usr)
	  ;; It has to be updated!
	  (bbdb-identica-query-update-record record usr)
	  (bbdb-display-records (cons record '()))
	  )
	)
    (progn
      ;; No record available... query to add it..
      (bbdb-identica-query-add-record record usr)
      ;; Show new record...
      (setq record 
	    (let ((usr-name (nth 1 usr)))
	      (car (bbdb-search (bbdb-records) usr-name))
	      )
	    )
      (when record
	(bbdb-display-records (cons record '()))
	)
      )
    )
  )

(defun bbdb-identica-query-update-record (record usr)
  "Query the user if she/he wants to update the BBDB record.
If she/he answer \"yes\", update it.
If she/he answer \"no\", do nothing."
  (when (bbdb-identica-prompt-yepnop "Do you want to update this record?(y/n)")
    (bbdb-identica-update-record record usr)
    )				     
)

(defun bbdb-identica-update-record (record usr)
  "Update the record usr with new values:
1) Update the \"identica\" field.
2) No need to update anything else..."
  (bbdb-record-set-note record 'identica (nth 0 usr))
  )

(defun bbdb-identica-prompt-yepnop (prompt)
  "Ask a question to the user for a yes-no answer.
Return t when user answer yes.
Return nil when user answer no."
  (let (
	(yepnop (read-char prompt)))
    (cond
     ((eq ?y yepnop)
      t)
     ((eq ?n yepnop)
      nil)
     (t 
      (message "Please, answer 'y' or 'n'.")
      (bbdb-identica-prompt-yepnop prompt))
     )
    )
  )


(defun bbdb-identica-query-add-record (record usr)
  "Query the user if she/he wants to add this identica user into BBDB.
If she/he answer \"yes\", add it.
If she/he answer \"no\", don't add it of course."
  (when (bbdb-identica-prompt-yepnop "Do you want to add this user and identica nick?(y/n)")
    (bbdb-identica-add-record usr)
    )	     
  )

(defun bbdb-identica-add-record (usr)
  "Add friend/follower into BBDB."  
  (bbdb-create-internal 
   (nth 1 usr) ;;name
   nil ;; affix
   nil ;; aka
   nil ;; organizations
   nil ;; mail
   nil ;; phones
   nil ;; addresses
   (cons 
    (cons 'identica (nth 0 usr))
    '()
    ) ;; notes
   )
  )

(defun bbdb-identica-check-record (record usr)
  "Check if the record has the same value in the \"identica\" field and the name field.
If it is the same return t.
If it is different return nil.
If the \"identica\" field does not exists return nil(it means it has different value).
"
  ;; Get if exists the field 
  (if (and 
       record
       usr)
      (string= 
       (bbdb-record-note record 'identica)
       (car usr)
       )
    nil
    )
  )

(defun bbdb-identica-ask-for-save ()
  "This is intended when the user wants to quit identica.
As soon he/she wants to quit, is necessary to ask if she/he wants to update BBDB database."
  (bbdb-save t t)
  )

(eval-and-compile
  (add-hook 'identica-friends-good-bye-hooks 'bbdb-identica-ask-for-save)
  (add-hook 'identica-friends-next-user-hooks 'bbdb-identica-friends-next-usr)
  (add-hook 'identica-friends-prev-user-hooks 'bbdb-identica-friends-next-usr)
  )

(defun bbdb-identica-next-usr ()
  "Go to next identica user in the identica buffer, find its BBDB record and show it if exists."
  (interactive)
  (save-excursion
    (goto-char (bbdb-identica-find-next-usr-pos))
    ;; Get user nick
    (save-excursion
      (search-forward-regexp "[^[:blank:]]+" nil t)
      (setq usrnick (match-string-no-properties 0))
      )
    ;; Remove the '@'
    (when (string= "@" (substring usrnick 0 1))
      ;;Has a '@', take it out.
      (setq usrnick (substring usrnick 0 1))
      )
    ;; Remove the ','
    (when (string= "," (substring usrnick -1))
      (setq usrnick (substring usrnick 0 -1))
      )  
    
    ;; Find usrnick in the BBDB
    (bbdb-search-notes "identica" usrnick)
    )
  )


(defun bbdb-identica-find-next-usr-1-pos ()
  "Find the next identica nick starting with '@'."
  (with-current-buffer identica-buffer
    (save-excursion
      (search-forward-regexp "@[^[:blank:]]*" nil t)
      (match-beginning 0)
      )
    )
  )

(defun bbdb-identica-find-next-usr-2-pos ()
  "Find the next identica nick as the first element that appear of a status. For example:

_
 rms,  10:26  septiembre 26, 2011:
  hola, esto es un estado // from web [algúnlado] in reply to someone

in this case the return value is 'rms'."
  (with-current-buffer identica-buffer
    (identica-get-next-username-face-pos (point))     
    )
  )

(defun bbdb-identica-find-next-usr-pos ()
  "Return the position of the first identica nick after the current point, no matters if it is a '@user' form or just
the name of the status's remitent."
  (let ((usr1 (bbdb-identica-find-next-usr-1-pos))
	(usr2 (bbdb-identica-find-next-usr-2-pos))
	)
    ;; Look wich one is first, and return that one
    (if (< usr1 usr2)
	usr1
      usr2
      )
    )
  )

(defun bbdb-identica-down-key ()
  "Go to down, and then show the next possible nick BBDB record."
  (interactive)
  (next-line)
  (bbdb-identica-next-usr)
  )

(defun bbdb-identica-up-key ()
  "Go to up and then show the next possible nick BBDB record."
  (interactive)
  (previous-line)
  (bbdb-identica-next-usr)
  )

;; I see that this could be a bit destructive.
;; If down or up key are setted to other functions, this will make identica to ignore them!

(eval-and-compile 
  ;; If you want, at every position, to search for BBDB uncomment this:
  ;;(define-key identica-mode-map [down] 'bbdb-identica-down-key)
  ;;(define-key identica-mode-map [up] 'bbdb-identica-up-key)
  )

;; This is better: at each j and k key(identica-goto-next-status) search for its BBDB record.
(defadvice identica-goto-next-status (after bbdb-identica-next-status)
  "Search for BBDB record of the next nearest nick."
  (save-excursion
    (backward-char)
    (bbdb-identica-next-usr)
    )
  )

(defadvice identica-goto-previous-status (after bbdb-identica-next-status)
  "Search for BBDB record of the next nearest nick."
  (save-excursion
    (backward-char)
    (bbdb-identica-next-usr)
    )
  )

(eval-and-compile
  (ad-activate 'identica-goto-next-status)
  (ad-activate 'identica-goto-previous-status)
  )

(provide 'bbdb-identica)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-identica.el ends here
