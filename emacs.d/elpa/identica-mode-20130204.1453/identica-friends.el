;;; identica-friends.el ---
;;
;; Filename: identica-friends.el
;; Description: A library that provides some functions to look who are your friends in your identi.ca account.
;; Author: Christian Giménez
;; Maintainer:
;; Created: dom sep 25 17:58:40 2011 (-0300)
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
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Use M-x identica first, if you are not connected, this library
;; will not work.
;; You can check who are your friends on Identi.ca using the function
;; M-x identica-show-friends
;; If you want to check who are following you, type:
;; M-x identica-show-followers
;;
;; I divided the code into sections. This sections are tabbed asside
;; and commented by an only one ";". Also are overlined and underlined
;; so, they are very visible.
;;
;; Convention:
;; All functions and variables in this modules has the prefix 
;; "identica-friends" so you can identify easyly. 
;; The main functions may not have this prefix so users don't get
;; confused.
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

(require 'xml)
(require 'identica-mode)

					; ____________________
					;
					; Variables
					; ____________________


(defvar identica-friends-buffer nil
  "Friend's Buffer. Internal use of identica-friends.el."
  )

(defvar identica-friends-buffer-name "*identica-friends*"
  "Friends buffer's name. Changing this variable will effect after you
recall identica-friends functions.
Be aware of no function or actual buffers exists. Reboot all identica-friends functions."
  )

(defvar identica-friends-buffer-type nil
  "If the buffer contains a list of users, the this is setted into 'users. 
If the buffer contains a list of groups, this is setted into 'groups.
Nil means, no buffer or just the programmer forgot to set it! :-S ."
  )

					; ----
					; Hooks Variables
					; ----

(defcustom identica-friends-good-bye-hooks
  'nil
  "These functions are called as soon as the `identica-friends-good-bye' functions finnish."
  :type '(hook)
  )


(defcustom identica-friends-mode-hooks
  'nil
  "These functions are called as soon as the `identica-friends-mode' functions finnish."
  :type '(hook)
  )


(defcustom identica-friends-show-friends-hooks
  'nil
  "These functions are called as soon as the `identica-show-friends' functions finnish."
  :type '(hook)
  )

(defcustom identica-friends-show-followers-hooks
  'nil
  "These functions are called as soon as the `identica-show-followers' functions finnish."
  :type '(hook)
  )

(defcustom identica-friends-next-user-hooks
  'nil
  "These functions are called as soon as the `identica-friends-next-user' functions finnish."
  :type '(hook)
  )

(defcustom identica-friends-prev-user-hooks
  'nil
  "These functions are called as soon as the `identica-friends-prev-user' functions finnish."
  :type '(hook)
  )




					; ____________________
					;
					; Faces and font-lock
					; ____________________

(defface identica-friends-mode-id
  '(
                                        ; If there's dark background...
    (((class color) (background dark))
     :foreground "yellow"
     )
                                        ; If there's light background...
    (((class color) (background light))
     :foreground "red"
     )

    (t :background "white"
       :foreground "blue")
    )
  ""
  )

(defface identica-friends-mode-bar
  '(
                                        ; If there's dark background...
    (((class color) (background dark))
     :bold t
     )
                                        ; If there's light background...
    (((class color) (background light))
     :bold t
     )

    (t :background "white"
       :foreground "blue"
       :bold t)
    )
  ""
  )

(defvar identica-friends-mode-font-lock
  '(
    ;; font-lock-keywords
    (
     ("^Id: .*$" . 'identica-friends-mode-id)
     ("^Nick: .*$" . 'identica-username-face)
     ("^--------------------$" . 'identica-friends-mode-bar)
     )

    ;; Otros...
    )
  ;;
  "Font lock for `identica-friends--mode'"
  )

					; ____________________
					;
					; Keymaps
					; ____________________

;; Keymaps calls functions from the "Interactive API Commands" sections(see below).

(defvar identica-friends-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'identica-friends-good-bye)
    (define-key map "n" 'identica-friends-next-user)
    (define-key map "p" 'identica-friends-prev-user)
    (define-key map [down] 'identica-friends-next-user)
    (define-key map [up] 'identica-friends-prev-user)
    (define-key map [left] 'identica-friends-prev-user)
    (define-key map [right] 'identica-friends-next-user)    
    (define-key map [return] 'identica-friends-goto-timeline-at-point)
    map)
  "Keymap for `identica-friends-mode'."
  )


					; ____________________
					;
					; Major Mode
					; ____________________


(define-derived-mode identica-friends-mode nil "Identica-friends-mode"
  "Major mode for identica-friends buffer.
Use `identica-show-friends' to call this buffer."
  ;; font lock para ej-mode
  (set (make-local-variable 'font-lock-defaults)
       identica-friends-mode-font-lock)
  (set (make-local-variable 'buffer-read-only) t)
  (make-local-variable 'inhibit-read-only)
  (run-hooks 'identica-friends-mode-hooks)
  )


					; ________________________________________
					;
					; Interactive API Commands
					; ________________________________________


(defun identica-friends-good-bye ()
  "Bury the *identica-friends* buffer"
  (interactive)
  (with-current-buffer identica-friends-buffer
    (bury-buffer)
    (run-hooks 'identica-friends-good-bye-hooks)
    )
  )

(defun identica-friends-next-user ()
  "Put the pointer in the next friend or follower in the identica-friend buffer."
  (interactive)
  (with-current-buffer identica-friends-buffer
    (goto-char (identica-friends-find-next-user-position))
    )
  (run-hooks 'identica-friends-next-user-hooks)
  )

(defun identica-friends-prev-user ()
  "Put the pointer in the previous friend or follower in the identica-friend buffer."
  (interactive)
  (with-current-buffer identica-friends-buffer
    (goto-char (identica-friends-find-prev-user-position))
    )
  (run-hooks 'identica-friends-prev-user-hooks)
  )

(defun identica-friends-goto-timeline-at-point ()
  "Check whenever we are in user-list or group-list. If we are listing user, call `identica-friends-goto-user-timeline-at-point', 
if not call `identica-friends-goto-group-timeline-at-point'."
  (interactive)
  (cond 
   ((eq identica-friends-buffer-type 'users)
    (identica-friends-goto-user-timeline-at-point)
    )
   ((eq identica-friends-buffer-type 'groups)
    (identica-friends-goto-group-timeline-at-point)
    )
   )
  )
   
(defun identica-friends-goto-user-timeline-at-point ()
  "Search for the username and go to his timeline."
  (interactive)
  (let ((username (identica-friends-find-username))
	)
    (identica-user-timeline username)
    (switch-to-buffer identica-buffer)
    )	
  )

(defun identica-friends-goto-group-timeline-at-point ()
  "Search for the group name and go to his timeline."
  (interactive)
  ;; Look that `identica-friends-find-username' can be used for getting anything that starts with the "Nick: " string,
  ;; so is usefull here as well!
  (let ((groupname (identica-friends-find-username))
	)
    (identica-group-timeline groupname)
    (switch-to-buffer identica-buffer)
    )	
  )
					;
					; Main function:
                                        ; Followers Commands
					;


(defun identica-show-followers()
  (interactive)
  (setq identica-friends-buffer-type 'users)
  (identica-http-get (sn-account-server sn-current-account)
		     (sn-account-auth-mode sn-current-account)
		     "statuses" "followers" nil 'identica-friends-show-user-sentinel '("follower"))    
  (run-hooks 'identica-friends-show-followers-hooks)
  )


                                        ;
					; Main function:
                                        ; Friends Commands
                                        ;


(defun identica-show-friends ()  
  (interactive)
;  (setq identica-method-class "statuses")
;  (setq identica-method "friends")
;  (identica-http-get identica-method-class identica-method identica-show-friend-sentinel)
  (setq identica-friends-buffer-type 'users)
  (identica-http-get (sn-account-server sn-current-account) ;; server
		     (sn-account-auth-mode sn-current-account);; auth-mode
		     "statuses" "friends" nil 'identica-friends-show-user-sentinel '("friend"))
  (run-hooks 'identica-friends-show-friends-hooks)
  )

(defun identica-show-groups ()  
  (interactive)
;;  (setq identica-method-class "statuses")
;;  (setq identica-method "friends")
;;  (identica-http-get identica-method-class identica-method identica-show-friend-sentinel)
  (setq identica-friends-buffer-type 'groups)
  (identica-http-get (sn-account-server sn-current-account) ;; server
		     (sn-account-auth-mode sn-current-account);; auth-mode
		     "statusnet" "groups/list" nil 'identica-friends-show-user-sentinel '("group"))
  ;;(run-hooks 'identica-friends-show-groups-hooks)
  )


					; ____________________
					;
					; Auxiliary Functions
					; ____________________


(defun identica-friends-find-username ()
  "Find the username in the nearby at current position.

I suppose that the cursor is on the nickname and not anywhere."
  (save-excursion
    (if (search-forward-regexp "Nick: \\(.*\\)" nil t)
	(match-string-no-properties 1)
      nil
      )
    )
  )

(defun identica-friends-buffer ()
  "Show a new buffer with all the friends. "
  (setq identica-friends-buffer (get-buffer-create identica-friends-buffer-name))
  (switch-to-buffer identica-friends-buffer)
  (identica-friends-mode)
  )


(defun identica-friends-get-current-user ()
  "Return the current user(friend or follower) that we are pointing now in the *identica-buffer*.
This will be returned as a list wich components are in these order:
 (NICK NAME DESCRIPTION LOCATION)"

  (setq usr '())
  (save-excursion
    ;; Position at the beginning of the user.
    (search-backward-regexp "^--------------------$" nil t)
    (goto-char (match-beginning 0))

    (setq usr (cons (identica-friends-get-location) usr))
    (setq usr (cons (identica-friends-get-desc) usr))

    (setq usr (cons (identica-friends-get-name) usr))
    (setq usr (cons (identica-friends-get-nick) usr))
    )
  usr
  )

(defun identica-friends-get-nick ()
  "Get the *next* user(friend or follower) nick.
If there are no user, return nil."
  (with-current-buffer identica-friends-buffer
    (save-excursion
      (search-forward-regexp "Nick: \\(.*\\)$" nil t)
      (match-string-no-properties 1)
      )
    )
  )

(defun identica-friends-get-name ()
  "Get the *next* user(friend or follower) nick.
If there are no user, return nil."
  (with-current-buffer identica-friends-buffer
    (save-excursion
      (search-forward-regexp "Name: \\(.*\\)$" nil t)
      (match-string-no-properties 1)
      )
    )
  )

(defun identica-friends-get-desc ()
  "Get the current user(friend or follower) nick.
If there are no user, return nil."
  (with-current-buffer identica-friends-buffer
    (save-excursion
      (search-forward-regexp "Description: \\(.*\\)$" nil t)
      (match-string-no-properties 1)
      )
    )
  )

(defun identica-friends-get-location ()
  "Get the current user(friend or follower) nick.
If there are no user, return nil."
  (with-current-buffer identica-friends-buffer
    (save-excursion
      (search-forward-regexp "Location: \\(.*\\)$" nil t)
      (match-string-no-properties 1)
      )
    )
  )

(defun identica-friends-show-user-sentinel
  (&optional status method-class method parameters type-of-user)
  "Sentinel executed after recieving all the information from identi.ca.
This sentinel needs to know if the TYPE-OF-USER(or type of list) is one of these:
- \"friend\"
- \"follower\"
- \"group\"

First, its parse the XML file recieved by identi.ca. While parsing, it show the user data into a buffer.

"
  ;; cnngimenez: This I used for debug HTTP
  (identica-friends-copiar-http-buffer)
  ;; Search for the begining of the xml...
  (goto-char (point-min))
  (search-forward "<?xml")
  (setq start-xml (match-beginning 0))
  ;; Parse xml into a list
  (setq lst-xml (xml-parse-region start-xml (point-max)))
  ;; Change buffer...
  (identica-friends-buffer)
  ;; Find elements on that list and write it
  (identica-friends-parse-xml-user-lists type-of-user lst-xml)
  
  (goto-char (point-min))
  )



(defun identica-friends-parse-xml-user-lists (type-of-user xml-lst)
  "Parse the xml-lst list and, after that, write every user into the current buffer.
The way it is parsed depends on the type-of-user we are talking about:
- \"friend\"
- \"follower\"
- \"group\"
"
  ;; Get first element
  (setq xml-lst (car xml-lst))
  ;; ignore the word 'users' and the next element... remove enter.
  (setq xml-lst (nthcdr 3 xml-lst))
  ;; Erase friends-buffer
  (with-current-buffer identica-friends-buffer-name
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      )
    )
  ;; for each user in the xml list, parse it, and write it...
  (dolist (usr xml-lst)
    (unless (stringp usr)
       (cond ((string= type-of-user "friend")
	      (identica-friends-write-user
	       (identica-friends-get-friend-data usr))) ;; Is a friend, parse xml as a friends.xml
	     ((string= type-of-user "follower")
	      (identica-friends-write-user
	       (identica-friends-get-follower-data usr)));; is a follower, parse as followers.xml
	     ((string= type-of-user "group")
	      (identica-friends-write-group
	       (identica-friends-get-group-data usr)))
	     )
       )
    )
  )



(defun identica-friends-write-user (usr-data)
  "Write an user taking the info from a list.
The list must be of the form given by the functions `identica-friends-get-friend-data'
or `identica-friends-get-follower-data':

 (id . name . screen_name . location . description )

"
  (let ((inhibit-read-only t))
    (insert-and-inherit "\nNick: " (decode-coding-string (nth 2 usr-data) 'utf-8))
    (insert-and-inherit "\nName: " (decode-coding-string (nth 1 usr-data) 'utf-8))
    (insert-and-inherit "\nDescription: " (decode-coding-string (nth 4 usr-data) 'utf-8))
    (insert-and-inherit "\nLocation: " (decode-coding-string (nth 3 usr-data) 'utf-8))
    (insert-and-inherit "\n--------------------\n")
    )
  )

(defun identica-friends-write-group (group-data)
  "Write a group taking the info from a list.
The list must be of the form given by the functions `identica-friends-get-group-data':

 (id url nickname fullname membership blocked member_count logo homepage description location modified)"
  (let ((inhibit-read-only t))
    (insert-and-inherit "\nNick: " (decode-coding-string (nth 2 group-data) 'utf-8))
    (insert-and-inherit "\nName: " (decode-coding-string (nth 3 group-data) 'utf-8))
    (insert-and-inherit "\nURL: " (decode-coding-string (nth 1 group-data) 'utf-8))    
    (insert-and-inherit "\nDescription: " (decode-coding-string (nth 9 group-data) 'utf-8))
    (insert-and-inherit "\nLocation: " (decode-coding-string (nth 10 group-data) 'utf-8))
    (insert-and-inherit "\nHomepage: " (decode-coding-string (nth 8 group-data) 'utf-8))
    (insert-and-inherit "\n--------------------\n")
    )
  )

;; *****
;; ** Comment about `identica-friends-get-follower-data' and `identica-friends-get-friend-data':
;;
;;   These parsers must be changed to a most suitable way of finding the members.
;;   Maybe using the "member" function or any simmilar makes a more reliable way of finding the attributes
;; than going to the nth element of the list.
;;
;;   This is because if we change the structure of the XML, or just alternate some items(for example: instead
;; using the description before the location, in the future the description comes after the location) this
;; functions won't work properly. Also, they aren't readable and easy to change.
;;
;; *****

(defun identica-friends-get-follower-data (usr-lst)
  "Parse the list and make a more easy-to-read list. The final list will have the following form suitable
for writing in a buffer with the function `identica-friends-write-user'.
  (id . name . screen_name . location . description )."

  (setq lst '())

  ;; Put the desription
  (push
   (nth 2 (nth 11 usr-lst))
   lst
   )

  ;; Put the location
  (push
   (nth 2 (nth 9 usr-lst))
   lst
   )

  ;; Put the screen-name
  (push
   (nth 2 (nth 7 usr-lst))
   lst)

  ;; Put the name
  (push
   (nth 2 (nth 5 usr-lst))
   lst)

  ;; Put the id
  (push
   (nth 2 (nth 3 usr-lst))
   lst)


  ;; Replace nils into strings...
  (replace-nils lst "")
  )


(defun identica-friends-get-friend-data (usr-lst)
  "Parse the list and make a list more easy to read. The list has the following form:
  (id . name . screen_name . location . description ).

This form is suitable for the function `identica-friends-write-user'.
"

  (setq lst '())

  ;; Put the desription
  (push
   (nth 2 (nth 11 usr-lst))
   lst
   )

  ;; Put the location
  (push
   (nth 2 (nth 9 usr-lst))
   lst
   )

  ;; Put the screen name
  (push
   (nth 2 (nth 7 usr-lst))
   lst
   )


  ;; Put the name
  (push
   (nth 2 (nth 5 usr-lst))
   lst
   )


  ;; Put the id
  (push
   (nth 2 (nth 3 usr-lst))
   lst
   )

  ;; Replace nils into strings...
  (replace-nils lst "")
  )

(defun identica-friends-get-group-data (usr-lst)
  "Parse the list and make a more easy-to-read list. The final list will have the following form suitable
for writing in a buffer with the function `identica-friends-write-user'.

 (id url nickname fullname membership blocked member_count logo homepage description location modified)."

  (setq lst '())

  ;; Put the id
  (push
   (nth 2 (nth 3 usr-lst))
   lst
   )

  ;; Put the url
  (push
   (nth 2 (nth 5 usr-lst))
   lst
   )

  ;; Put the nickname
  (push
   (nth 2 (nth 7 usr-lst))
   lst)

  ;; Put the fullname
  (push
   (nth 2 (nth 9 usr-lst))
   lst)

  ;; Put the membership
  (push
   (nth 2 (nth 11 usr-lst))
   lst)

  ;; Put the blocked
  (push
   (nth 2 (nth 13 usr-lst))
   lst)

  ;; Put the member-count
  (push
   (nth 2 (nth 15 usr-lst))
   lst)

  ;; Put the logo(original logo!)
  (push
   (nth 2 (nth 17 usr-lst))
   lst)

  ;; Put the homepage
  (push
   (nth 2 (nth 25 usr-lst))
   lst)

  ;; Put the description
  (push
   (nth 2 (nth 27 usr-lst))
   lst)

  ;; Put the location
  (push
   (nth 2 (nth 29 usr-lst))
   lst)

  ;; Put the modified
  (push
   (nth 2 (nth 33 usr-lst))
   lst)

  (setq lst (reverse lst))

  ;; Replace nils into strings...
  (replace-nils lst "")
  )


(defun identica-friends-find-next-user-position ()
  "Find the position in the *identica-friend-buffer* of the next user. If there are no next user(we are at the end of the list)
return the first one.
This function return nil when there are any friend in the buffer."
  (with-current-buffer identica-friends-buffer
    ;; We have to put one char forward so, we cannot detect the actual "Nick: "
    (forward-char 1)
    (if (search-forward-regexp "Nick: " nil t)
	(match-beginning 0)
      (progn
	;; Not found! Maybe we are at the end?
	;; Go to the beginning of the buffer and search again, if fails, this user has no friends!
	(goto-char (point-min))
	(if (search-forward-regexp "Nick: " nil t)
	    (match-beginning 0) ; Yes, he has friends... the pointer was at the end of buffer
	  'nil ; Wow... he has no friends!
	  )
	)
      )
    )
  )
(defun identica-friends-find-prev-user-position ()
  "Find the position in the *identica-friend-buffer* of the previous user. If there are no previous user(we are at the begin of the list)
return the last one.
This function return nil when there are any friend in the buffer."
  (with-current-buffer identica-friends-buffer
    (if (search-backward-regexp "Nick: " nil t)
	(match-beginning 0)
      (progn
	;; Not found! Maybe we are at the end?
	;; Go to the beginning of the buffer and search again, if fails, this user has no friends!
	(goto-char (point-max))
	(if (search-backward-regexp "Nick: " nil t)
	    (match-beginning 0) ; Yes, he has friends... the pointer was at the end of buffer
	  'nil ; Wow... he has no friends!
	  )
	)
      )
    )
  )
                                        ; ____________________
					;
                                        ; Commons Functions
                                        ; ____________________


;; Are there any function to replace anything from a list?
(defun replace-nils (lst elt)
  "Replace nils with an element elt."
  (unless (null lst)
    (if (null (car lst))
        (cons elt (replace-nils (cdr lst) elt))
      (cons (car lst) (replace-nils (cdr lst) elt))
      )
    )
  )




                                        ;
                                        ; For debugging purpose
                                        ;



(defvar identica-friends-http-debug "*identica-http*"
  "Buffer to the http requests")

(defun identica-friends-copiar-http-buffer ()
  "Copia el buffer http a otro nuevo para ver el resultado de la comunicación."
  (with-current-buffer identica-http-buffer
    (setq str (buffer-string))
    )
  (with-current-buffer (get-buffer-create identica-friends-http-debug)
    (delete-region (point-min) (point-max))
    (insert str)
    )
  )


(provide 'identica-friends)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; friends.el ends here
