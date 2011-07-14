;;; sb-nikkansports.el --- shimbun backend for www.nikkansports.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007, 2008
;; MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-nikkansports
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkansports-url "http://www.nikkansports.com/")

(defvar shimbun-nikkansports-server-name "$BF|4)%9%]!<%D(B")

(defvar shimbun-nikkansports-group-table
  '(("flash" "$B:G?7%K%e!<%9(B" "flash/flash-news.html")
    ("baseball" "$BLn5e(B" "baseball/news/backnumber-baseball.html")
    ("baseball.highschool" "$B9b9;Ln5e(B"
     "baseball/highschool/news/backnumber-highschool.html")
    ("baseball.amateur" "$BBg3X!&<R2q?MLn5e(B"
     "baseball/amateur/news/backnumber-amateur.html")
    ("baseball.mlb" "$B#M#L#B(B" "baseball/mlb/news/backnumber-mlb.html")
    ("soccer" "$B%5%C%+!<(B" "soccer/news/backnumber-soccer.html")
    ("soccer.japan" "$B%5%C%+!<F|K\BeI=(B"
     "soccer/japan/news/backnumber-japan.html")
    ("soccer.world" "$B3$30%5%C%+!<(B" "soccer/world/news/backnumber-world.html")
    ("sports" "$B%9%]!<%D(B" "sports/news/backnumber-sports.html")
    ("sumo" "$BBgAjKP(B" "sports/sumo/news/backnumber-sumo.html")
    ("nba" "$B#N#B#A(B" "sports/nba/news/backnumber-nba.html")
    ("nfl" "$B#N#F#L(B" "sports/nfl/news/backnumber-nfl.html")
    ("nhl" "$B#N#H#L(B" "sports/nhl/news/backnumber-nhl.html")
    ("rugby" "$B%i%0%S!<(B" "sports/rugby/news/backnumber-rugby.html")
    ("golf" "$B%4%k%U(B" "sports/golf/news/backnumber-golf.html")
    ("motor" "$B%b!<%?!<%9%]!<%D(B" "sports/motor/news/backnumber-motor.html")
    ("battle" "$B3JF.5;(B" "battle/news/backnumber-battle.html")
    ("race" "$B6%GO(B" "race/news/backnumber-race.html")
    ("race.kka" "$B6%NX!&6%Dz!&%*!<%H(B" "race/kka/news/backnumber-kka.html")
    ("entertainment" "$B7]G=(B" "entertainment/news/backnumber-entertainment.html")
    ("cinema" "$B%7%M%^(B" "entertainment/cinema/news/backnumber-cinema.html")
    ("general" "$B<R2q(B" "general/news/backnumber-general.html")))

(defvar shimbun-nikkansports-content-start
  "<[\t\n ]*![\t\n ]*-+[\t\n ]*\\++[\t\n ]*\
$B%K%e!<%9K\J8(B[\t\n ]*\\++[\t\n ]*-+[\t\n ]*>[\t\n ]*\
\\(?:\\(?:<[\t\n ]*/?[\t\n ]*[ads][^>]+>\
\\|<[\t\n ]*h[0-9]+[\t\n ]*>[^<]+<[\t\n ]*/[\t\n ]*h[0-9]+[\t\n ]*>\\)\
\[\t\n ]*\\)*")

(defvar shimbun-nikkansports-content-end
  "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
<[\t\n ]*![\t\n ]*-+[\t\n ]*/[\t\n ]*\\++[\t\n ]*\
$B%K%e!<%9K\J8(B[\t\n ]*\\++[\t\n ]*-+[\t\n ]*>")

(defvar shimbun-nikkansports-expiration-days 17)

(defvar shimbun-nikkansports-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAgMAAABinRfyAAAADFBMVEUDKpy11PIeeNv///+
 PA1z5AAAAP0lEQVQI12NgAAMOJgYG/n/2Fxj4a+0+MPDH2i4AEvYPgAQ3iLvvB5DYCyL0LzCsAgI
 kYm3u+yqGte9Td4G5AJiKHahMk6/LAAAAAElFTkSuQmCC")))

(luna-define-method shimbun-groups ((shimbun shimbun-nikkansports))
  (mapcar 'car shimbun-nikkansports-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkansports))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkansports-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkansports))
  (shimbun-expand-url (nth 2 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-nikkansports-group-table))
		      shimbun-nikkansports-url))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkansports)
					 &optional range)
  (shimbun-nikkansports-get-headers shimbun range))

(defun shimbun-nikkansports-get-headers (shimbun range)
  (let ((regexp
	 (eval-when-compile
	   (concat
	    "<a[\t ]+href=\""
	    ;; 1. url
	    "\\([^\"]+/"
	    ;; 2. serial number
	    "\\([^/]+"
	    ;; 3. year
	    "\\(20[0-9][0-9]\\)"
	    ;; 4. month
	    "\\([01][0-9]\\)"
	    ;; 5. day
	    "\\([0-3][0-9]\\)"
	    "[^.]+\\)\\.html\\)\">[\t ]*"
	    ;; 6. subject
	    "\\([^<]+\\)"
	    "[\t ]*</a>[^\n]+\\[[\t ]*[0-9]+$BF|(B[\t ]*"
	    ;; 7. time
	    "\\([012][0-9]:[0-5][0-9]\\)"
	    "[\t ]*\\]")))
	(group (shimbun-current-group-internal shimbun))
	(from (concat shimbun-nikkansports-server-name " ("
		      (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	headers)
    (setq group (mapconcat 'identity
			   (nreverse (split-string group "\\."))
			   "."))
    (while (re-search-forward regexp nil t)
      (push (shimbun-create-header
	     0 (match-string 6) from
	     (shimbun-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (match-string 7))
	     (concat "<"
		     (mapconcat 'identity
				(save-match-data
				  (split-string (match-string 2) "-"))
				".")
		     "%" group ".nikkansports.com>")
	     "" 0 0 (match-string 1))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-nikkansports)
						    header)
  (shimbun-strip-cr)
  (goto-char (point-min))
  (when (luna-call-next-method)
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-nikkansports)

;;; sb-nikkansports.el ends here
