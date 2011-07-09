;; Spanish minor mode - easy typing of Spanish accented characters

;; Copyright (C) 1999  Philip Dorrell
     
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License
;;     as published by the Free Software Foundation; either version 2
;;     of the License, or (at your option) any later version.
     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
     
;;     A copy of the GNU General Public License is included with the
;;     GNU Emacs distribution.

;; Email: pdorrell@.com      (add "pobox" after "@" and before ".com").
;; (The previous line is to confuse spambots trying to read this page.)
;; Web: www.1729.com

;; Standard GNU emacs contains two main functions that help with
;; editing accented non-English languages:

;; standard-european, which is used here, to display higher characters
;; as accented characters

;; iso-accents-mode, which allows one to use keys `'"^/~ as special
;; accent keys to compose accented characters

;; In Spanish, the following is almost true:
;; For every accented or non-ASCII character, there is one
;; corresponding ASCII character, and no ASCII character has more
;; than one corresponding non-ASCII character. The exceptions are
;; the raised a's and o's use with ordinal numbers.
;; spanish-minor-mode defines two toggle keys (here set to be F2 and F3).
;; F3 toggles a and o to the ordinal endings, F2 toggles all other
;; possibilities with characters AaEeIiNnOoUu!?
;; F2 and F3 toggle the characters preceding the cursor, i.e. the 
;; character just typed, so to type ¿ Como estás ?, type as follows:
;; ? F2 (space) C o m o (space) e s t a F2 s ?
;; It is also easy to add accents to text previously typed without
;; accents (move cursor position following character, use F2 or F3 to toggle).

;; To make this mode accessible, add the following lines (uncommented)
;; to your _emacs file or to "site-lisp/default.el":

;;(autoload 'spanish-minor-mode "spanish")
;;(global-set-key [?\M-\C-S] 'spanish-minor-mode)

;; Shift-Meta-Control S will then toggle spanish-minor-mode
;; (= Shift-Alt-Control S on Win 95/NT)
;-----------------------------------------------------------------
;; The next two functions are utility routines that help you to see
;; which character is which. They are not needed to make the
;; Spanish mode work.
;;
;; jeff mickey <j@codemac.net>
;; Converted to esperanto!

(defun octal-digits (n)
  "Return NUMBER as a string of octal digits"
  (let ( (n1 n) (x 0) (digits nil) (len 0))
    (while (> n1 0)
      (setq n2 (/ n1 8))
      (setq digit (- n1 (* 8 n2)))
      (setq digits (cons digit digits))
      (setq x (+ (* x 10) digit))
      (setq len (1+ len))
      (setq n1 n2) )
    (setq string (make-string len 0))
    (dotimes (i len)
      (aset string i (+ ?0 (nth i digits))))
    string))

(defun show-char-codes ()
  "Show all characters and codes in a buffer"
  (interactive)
  (let ( (output (get-buffer-create "*characters*")))
    (switch-to-buffer-other-window output)
    (delete-region (point-min) (point-max))
    (dotimes (i 64)
      (dotimes (j 8)
	(let ( (n (+ (* i 8) j)) )
	  (insert (octal-digits n) ": " n " ")))
      (insert "\n"))))

;-----------------------------------------------------------------
(defun make-toggle-array (&rest strings)
  "Make a lookup array to toggle characters around the STRINGS"
  (let ((toggle-array (make-vector 1024 nil)))
    (dolist (string strings)
      (let ( (len (length string)) j)
	(dotimes (i len)
	  (setq j (1+ i))
	  (if (>= j len) (setq j 0))
	  (aset toggle-array (aref string i) (aref string j)))))
    toggle-array))

(defvar esperanto-accents-toggler
  (make-toggle-array
   "C\410" "G\434" "H\444" "J\464" "S\534" "U\554" 
   "c\411" "g\435" "h\445" "j\465" "s\535" "u\555")
  "Array to toggle normal ASCII characters with most
common Esperanto variants")

(defun toggle-character (toggle-array)
  "Toggle character just before cursor according to
toggle array"
  (let ( (ch (char-after (1- (point)))) new)
    (if ch
	(progn
	  (setq new (aref toggle-array ch))
	  (if new
	      (progn 
		(delete-backward-char 1)
		(insert (make-string 1 new)))
	    (message 
	     (concat "No toggle value for character " (make-string 1 ch)))))
      (message "No character to toggle"))))

(defun esperanto-toggle-accent ()
  "Toggle Esperanto accent"
  (interactive)
  (toggle-character esperanto-accents-toggler))

(define-minor-mode eo-mode
  "Minor mode providing easy access to character accents for Esperanto"
  nil
  " eo"
  `(
    (,(kbd "x") . esperanto-toggle-accent)
    )
  )







;(defvar esperanto-minor-mode nil
;  "If NON-nil, x will toggle Esperanto accents" )
;
;(defvar esperanto-minor-mode-keymap
;  (make-sparse-keymap))
;
;(if esperanto-minor-mode-keymap
;    (progn
;      (define-key esperanto-minor-mode-keymap (kbd "x") 'esperanto-toggle-accent)))
;
;(if (not (assq 'esperanto-minor-mode minor-mode-alist))
;    (setq minor-mode-alist
;	  (append minor-mode-alist '((esperanto-minor-mode " eo")))))
;
;(if (not (assq 'esperanto-minor-mode minor-mode-map-alist))
;    (setq minor-mode-map-alist
;	  (cons (cons 'esperanto-minor-mode esperanto-minor-mode-keymap)
;		minor-mode-map-alist)))
;
;(defun esperanto-minor-mode ()
;  "Turn on x toggling of Esperanto accents"
;  (interactive)
;  (if esperanto-minor-mode
;      (progn
;	(setq esperanto-minor-mode nil))
;    (progn
;      (setq esperanto-minor-mode t)))
;  (redraw-display))
