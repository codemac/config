;;; elmo-date.el --- Date processing module for ELMO.

;; Copyright (C) 1998,1999,2000 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;;; Code:
;;


(require 'path-util)
(require 'timezone)
(require 'elmo-vars)

(defmacro elmo-match-substring (pos string from)
  "Substring of POSth matched string of STRING."
  (` (substring (, string)
		(+ (match-beginning (, pos)) (, from))
		(match-end (, pos)))))

(defmacro elmo-match-string (pos string)
  "Substring POSth matched STRING."
  (` (substring (, string) (match-beginning (, pos)) (match-end (, pos)))))

(defmacro elmo-match-buffer (pos)
  "Substring POSth matched from the current buffer."
  (` (buffer-substring-no-properties
      (match-beginning (, pos)) (match-end (, pos)))))

;; from subr.el
(defun elmo-replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string.
And returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	     rtn-str
	     (substring str prev-start match)
	     (cond (literal newtext)
		   (t (mapconcat
		       (function
			(lambda (c)
			  (if special
			      (progn
				(setq special nil)
				(cond ((eq c ?\\) "\\")
				      ((eq c ?&)
				       (elmo-match-string 0 str))
				      ((and (>= c ?0) (<= c ?9))
				       (if (> c (+ ?0 (length
						       (match-data))))
					   ;; Invalid match num
					   (error "Invalid match num: %c" c)
					 (setq c (- c ?0))
					 (elmo-match-string c str)))
				      (t (char-to-string c))))
			    (if (eq c ?\\) (progn (setq special t) nil)
			      (char-to-string c)))))
		       newtext ""))))))
    (concat rtn-str (substring str start))))

(defvar elmo-date-descriptions
  '((yesterday . [0 0 1])
    (lastweek  . [0 0 7])
    (lastmonth . [0 1 0])
    (lastyear  . [1 0 0])))

(defun elmo-date-get-description (datevec)
  (format "%d-%s-%d"
	  (aref datevec 2)
	  (car (rassq (aref datevec 1)
		      timezone-months-assoc))
	  (aref datevec 0)))

(defun elmo-date-get-datevec (description)
  (cond
   ((not elmo-date-match)
    (error "Date match is not available"))
   ((string-match "^[ \t]*\\([0-9]+\\)?[ \t]*\\([a-zA-Z]+\\)$" description)
    (let ((today
	   (save-match-data
	     (timezone-fix-time (current-time-string) (current-time-zone)
				nil)))
	  (number
	   (string-to-int
	    (if (match-beginning 1)
		(elmo-match-string 1 description)
	      "0")))
	  (suffix (downcase (elmo-match-string 2 description)))
	  pair)
      (if (setq pair (assq (intern suffix) elmo-date-descriptions))
	  (elmo-datevec-substitute today (cdr pair))
	(if (string= "daysago" suffix)
	    (elmo-date-get-offset-datevec today number)
	  (error "%s is not supported yet" suffix)))))
   ((string-match "[0-9]+-[A-Za-z]+-[0-9]+" description)
    (timezone-fix-time
     (concat (elmo-replace-in-string description "-" " ") " 0:0")
     (current-time-zone) nil))
   ((string-match "\\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\)" description)
    (vector (string-to-number (match-string 1 description))
	    (string-to-number (match-string 2 description))
	    (string-to-number (match-string 3 description))
	    0 0 0
	    (current-time-zone)))))

(defun elmo-datevec-substitute (datevec1 datevec2)
  (if (/= (aref datevec2 2) 0)
      (elmo-date-get-offset-datevec datevec1 (aref datevec2 2))
    (let ((year (- (aref datevec1 0) (aref datevec2 0)))
	  (month (- (aref datevec1 1) (aref datevec2 1)))
	  (timezone (current-time-zone)))
      (while (<= month 0)
	(setq year (1- year)
	      month (+ 12 month)))
      (timezone-fix-time
       (format "%d %s %d 0:00 %s"
	       (aref datevec1 2)
	       (car (rassq month timezone-months-assoc))
	       year
	       (cadr timezone)) nil nil))))

(defun elmo-date-get-week (year month mday)
  (let ((wday (symbol-value (intern (format
				     "elmo-weekday-name-%s"
				     elmo-lang))))
	y1 days p)
    (setq y1 (- year 1))
    (setq days (- (+ (* y1 365) (/ y1 400) (/ y1 4)) (/ y1 100)))
    (setq p 1)
    (while (< p month)
      (setq days (+ days (timezone-last-day-of-month p year)))
      (setq p (+ p 1)))
    (setq days (+ days mday))
    (aref wday (% days 7))))

(defun elmo-date-get-offset-datevec (datevec offset &optional time)
  (let ((year  (aref datevec 0))
	(month (aref datevec 1))
	(day   (aref datevec 2))
	(hour     (aref datevec 3))
	(minute   (aref datevec 4))
	(second   (aref datevec 5))
	(timezone (aref datevec 6))
	day-number p
	day-of-month)
    (setq p 1)
    (setq day-number (- (timezone-day-number month day year)
			offset))
    (while (<= day-number 0)
      (setq year (1- year)
	    day-number (+ (timezone-day-number 12 31 year)
			  day-number)))
    (while (> day-number (setq day-of-month
			       (timezone-last-day-of-month p year)))
      (setq day-number (- day-number day-of-month))
      (setq p (1+ p)))
    (setq month p)
    (setq day day-number)
    (timezone-fix-time
     (format "%d %s %d %s %s"
	     day
	     (car (rassq month timezone-months-assoc))
	     year
	     (if time
		 (format "%d:%d:%d" hour minute second)
	       "0:00")
	     (cadr timezone)) nil nil)))

(defmacro elmo-date-make-sortable-string (datevec)
  "Make a sortable string from DATEVEC."
  (` (timezone-make-sortable-date
      (aref (, datevec) 0)
      (aref (, datevec) 1)
      (aref (, datevec) 2)
      (timezone-make-time-string
       (aref (, datevec) 3)
       (aref (, datevec) 4)
       (aref (, datevec) 5)))))

(require 'product)
(product-provide (provide 'elmo-date) (require 'elmo-version))

;;; elmo-date.el ends here
