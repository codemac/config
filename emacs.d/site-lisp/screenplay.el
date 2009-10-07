;;; screenplay.el --- A major mode for editing screenplay files.

;; $Id: screenplay.el,v 1.1 2004/08/27 19:24:03 vls Exp $

;; Copyright (C) 2000, 2001, 2002, 2003, 2004  Vance L. Simpson

;; Author: V. L. Simpson <vls@freeshell.org>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; Massive complete start-over re-write back-to-the-drawing-board
;; start-from-square-one bottom-of-the-barrel-looking-up release.
;; 
;; Screenplay mode is designed as a submission draft editor.  Which
;; means that it'll make something you can send to Speilberg to be
;; rejected as unsuitable by some schmuck first reader.

;; Latest version is always available at:
;; http://www.nongnu.org/screenplay/files/screenplay.el
;; http://savannah.nongnu.org/cvs/?group=screenplay

;; Installing and using screenplay.el:
;; Put this file somewhere on your emacs load-path.
;; Load the file with 'load-libray RET screenplay RET'.
;; Open up your Academy Award Winner(TM), M-x screenplay-mode and have
;; at it.
;; The TAB and RET keys let you insert and edit the basic screenplay
;; elements: scene headings; action blocks; dialog blocks.
;;
;; TAB-RET asks for and inserts a scene heading, e.g., INT. HOUSE -- DAY.
;;
;; TAB-TAB-RET moves into action block mode, e.g., Describing the
;; house exploding.
;;
;; TAB-TAB-TAB-RET does the dialog thing, e.g.,
;;         BOB
;;   Gee, the house just
;;   exploded.
;;
;; To edit a pre-existing screenplay element, place cursor within that
;; element and execute the appropriate command with a prefix argument:
;; .e.g., C-u TAB-TAB RET will reset left and right margins as needed
;; for an action block.

;; Bugs and caveats:
;;
;; Don't enter any spurious newlines when finished editing any one
;; particular element.  Just hit the key combo for the next thing you
;; want to do, e.g., INT. HOUSE -- DAY(Type TAB-TAB-RET to go into an
;; action block.
;;

;; Remember kiddies:
;; "Nobody knows anything" -- William Goldman _Adventures in the Screentrade_

;; TODO:  Better documentation.  
;; Implement project handling:  Revision control, state control, etc.
;; Make the editing commands work from any point within a screenplay
;; element instead of end-of-line only.
;; Get all hard coded values into def{custom,var}'s

;; Write a frikkin' screenplay instead of procrastinating with this
;; thing.

;;; Code:

(defconst screenplay-version "0.7.0"
  "Current Emacs Screenplay Mode version number.")
(defconst screenplay-author-name  "V. L. Simpson")
(defconst screenplay-author-email "vls@freeshell.org")
(defconst screenplay-web-page     "http://www.nongnu.org/screenplay/")
(defconst screenplay-bug-address
  "screenplay-bug@nongnu.org"
  "Bug reports for Screenplay Mode go here.")

(defgroup screenplay nil
  "Screenplay editing."
  :group 'applications
  :link '(emacs-commentary-link :tag "Help" "screenplay"))

;; FIXME: Not supposed to do this but easiest way to handle filling at
;; the moment.  May implement the old re-filling code from old version.
(defcustom screenplay-mode-hook 'auto-fill-mode
  "List of functions to call when entering Screenplay Mode."
  :type 'hook
  :group 'screenplay)

(defcustom screenplay-left-margin 0
  "Left margin for scene headings and action blocks" 
  :type 'integer
  :group 'screenplay)

(defcustom screenplay-right-margin 50
  "Right margin for scene-headings and action blocks"
  :type 'integer
  :group 'screenplay)

;; I'll give internal variables and defuns 'scrn' prefix.
(defvar scrn-scene-hist ()
  "History list for scene headings.")

(defvar scrn-dialog-name-hist ()
  "History list for dialog block name attribute.")

(define-derived-mode screenplay-mode fundamental-mode "Screenplay"
  "Major mode for editing screenplays.
\\{screenplay-mode-map}"
;; FIXME: Try some kind of command rotation scheme with just tab and enter.
  (define-key screenplay-mode-map "\t\r" 'screenplay-slugline)
  (define-key screenplay-mode-map "\t\t\r" 'screenplay-action-block)
  (define-key screenplay-mode-map "\t\t\t\r" 'screenplay-dialog-block)
  (make-local-variable 'scrn-scene-hist)
  (make-local-variable 'screenplay-right-margin)
  (make-local-variable 'screenplay-left-margin)
  (make-local-variable 'scrn-dialog-name-hist)
  )

(defun scrn-margins ()
  "Set left-margin and fill-column for slugline and action blocks."
  (setq left-margin screenplay-left-margin)
  (setq fill-column screenplay-right-margin))

(defun screenplay-read-slugline ()
  "Get scene heading.
Returns scene heading in upper-case format."
  (let ((scene-heading 
         (let ((prompt "Enter scene heading: "))
           (read-from-minibuffer prompt 
                                 nil           ;initial-contents
                                 nil           ;keymap
                                 nil           ;read
                                 'scrn-scene-hist   ;hist
                                 nil           ;default
                                 nil))))       ;inherit-input-method
    (upcase scene-heading)))

(defun scrn-edit-slugline ()
  (cond (current-prefix-arg
         (scrn-margins)
         nil)
        (t
         (screenplay-read-slugline))))

(defun screenplay-slugline (scene)
  "Insert a scene heading.
To edit an existing scene heading, put the cursor on that line
and call this function with a prefix-arg, i.e, C-u TAB-RET."
  (interactive (list (scrn-edit-slugline)))
  ;;  (interactive (list (screenplay-read-slugline)))
  (cond ((not scene)
         nil)
        (t
         (newline 2)
         (scrn-margins)
         (indent-to-left-margin)
         (insert scene))))

(defun screenplay-action-block ()
  "Edit a description block.
With a prefix argument, just set margins and fill-column for an
action block element."
  (interactive)
  (cond (current-prefix-arg
         (scrn-margins))
        (t
         (newline 2)
         (scrn-margins)
         (use-hard-newlines -1)
         (indent-to-left-margin))))

(defun screenplay-dialog-char-name ()
"Return uppercase dialog block character tag."
  (let ((char-name
         (let ((prompt "Enter character name: "))
           (read-from-minibuffer prompt
                                 nil
                                 nil
                                 nil
                                 'scrn-dialog-name-hist
                                 nil
                                 nil))))
    (upcase char-name)))

(defvar screenplay-dialog-left-margin 10)
(defvar screenplay-dialog-right-margin 40)

(defun scrn-dialog-margins ()
  (setq left-margin screenplay-dialog-left-margin)
  (setq fill-column screenplay-dialog-right-margin))

(defun scrn-edit-dialog ()
  (cond (current-prefix-arg
         (scrn-dialog-margins)
         (use-hard-newlines 1 t)
         nil)
        (t
         (screenplay-dialog-char-name))))

(defun screenplay-dialog-block (name)
  "Edit dialog block."
  (interactive (list (scrn-edit-dialog)))
  (cond ((not name)
         nil)
        (t
         (use-hard-newlines 1 t)
         (newline 2)
         (setq left-margin 20)
         (indent-to-left-margin)
         (insert name)
         (newline 1)
         (setq left-margin 10)
         (setq fill-column 40)
         (indent-to-left-margin))))

(defun screenplay-version ()
  "Display current program version in echo area."
  (interactive)
  (message "Screenplay Mode Version %s" screenplay-version))

(defun screenplay-submit-bug-report ()
  "Submit a bug report for Screenplay Mode."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   screenplay-bug-address
   (concat "screenplay-" screenplay-version)
   nil
   nil
   nil
   "Please make your report as detailed as possible.
I'll try to fix it as soon as possible.

Thanks,
vls
Emacs Screenplay Mode
http://www.nongnu.org/screenplay/"))

(provide 'screenplay)
;;; screenplay.el ends here
