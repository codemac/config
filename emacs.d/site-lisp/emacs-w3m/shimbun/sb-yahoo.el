;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007, 2009, 2010
;; Kazuyoshi KOREEDA

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yahoo (shimbun) ())

(defvar shimbun-yahoo-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-yahoo-url "http://headlines.yahoo.co.jp/")

(defvar shimbun-yahoo-groups-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"\\(?:[^\"]+\\)?"
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0 "\\(?:<strong>" s0 "\\)?"
		    ;; 6. subject
		    "\\([^<]+\\)"
		    "\\(?:" s0 "</strong>\\)?"
		    s0 "</a>\\(?:\\(?:[^\n<$B!J(B]*\\|[\t\n ]*\\)<[^>]+>\\)*" s0
		    "\\(?:$B!J(B" s0 "\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 7. source
		    "\\([^<$B!K(B]+\\)"
		    s0 "\\(?:</a>" s0 "\\)?"
		    s0 "$B!K(B"
		    "\\(?:" s0 "-\\(?:[^<]+\)\\)?\
\\|" s0 "\\(?:<[^>]+>" s0 "\\)?\
\\(?:[01]?[0-9]$B7n(B\\)?[0-3]?[0-9]$BF|(B\\(?:([$BF|7n2P?eLZ6bEZ(B])\\)?\\)?\
\\|[01]?[0-9]$B7n(B[0-3]?[0-9]$BF|(B\\(?:([$BF|7n2P?eLZ6bEZ(B])\\)?\\)"
		    s0
		    ;; 8. hour
		    "\\([012]?[0-9]\\)"
		    s0 "$B;~(B" s0
		    ;; 9. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "$BJ,(B"
		    "\\(?:\\(?:" s0 "</[^>]+>\\)?[^<]+<a" s1
		    "href=\"[^\">]+\">" s0
		    ;; 10. source
		    "\\([^<$B!K(B]+\\)"
		    s0 "</a>\\)?")
		   1 2 3 4 5 6 7 8 9 10))
	 (topnews (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0 "</a>\\(?:" s0 "<[^>]+>\\)+" s0
		    ;; 7. source
		    "\\([^<]+\\)"
		    ".+)" s0
		    ;; 8. hour
		    "\\([012]?[0-9]\\)"
		    s0 "$B;~(B" s0
		    ;; 9. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "$BJ,G[?.(B")
		   1 2 3 4 5 6 7 8 9)))
    `(("topnews" "$B%H%C%W(B" "topnews" ,@topnews)
      ("news" "$B%K%e!<%9(B" news ,@default)
      ("politics" "$B@/<#(B" "pol" ,@default)
      ("society" "$B<R2q(B" "soci" ,@default)
      ("people" "$B?M(B" "peo" ,@default)
      ("business-all" "$B7P:QAm9g(B" "bus_all" ,@default)
      ("market" "$B;T67(B" "brf" ,@default)
      ("stock" "$B3t<0(B" "biz" ,@default)
      ("industry" "$B;:6H(B" "ind" ,@default)
      ("international" "$B3$30(B" "int" ,@default)
      ("entertainment" "$B%(%s%?!<%F%$%s%a%s%H(B" "ent" ,@default)
      ("sports" "$B%9%]!<%D(B" "spo" ,@default)
      ("computer" "$B%3%s%T%e!<%?(B" "sci" ,@default)
      ("zenkoku" "$BA49q(B" "loc" ,@default)
      ("hokkaido" "$BKL3$F;(B" "hok" ,@default)
      ("aomori" "$B@D?9(B" "l02" ,@default) ;; not "102" but "l02" ;-)
      ("iwate" "$B4d<j(B" "l03" ,@default)
      ("miyagi" "$B5\>k(B" "l04" ,@default)
      ("akita" "$B=)ED(B" "l05" ,@default)
      ("yamagata" "$B;37A(B" "l06" ,@default)
      ("fukushima" "$BJ!Eg(B" "l07" ,@default)
      ("tokyo" "$BEl5~(B" "l13" ,@default)
      ("kanagawa" "$B?@F`@n(B" "l14" ,@default)
      ("chiba" "$B@iMU(B" "l12" ,@default)
      ("saitama" "$B:k6L(B" "l11" ,@default)
      ("ibaraki" "$B0q>k(B" "l08" ,@default)
      ("tochigi" "$BFJLZ(B" "l09" ,@default)
      ("gunma" "$B72GO(B" "l10" ,@default)
      ("yamanashi" "$B;3M|(B" "l19" ,@default)
      ("nagano" "$BD9Ln(B" "l20" ,@default)
      ("niigata" "$B?73c(B" "l15" ,@default)
      ("toyama" "$BIY;3(B" "l16" ,@default)
      ("ishikawa" "$B@P@n(B" "l17" ,@default)
      ("fukui" "$BJ!0f(B" "l18" ,@default)
      ("aichi" "$B0&CN(B" "l23" ,@default)
      ("gifu" "$B4tIl(B" "l21" ,@default)
      ("shizuoka" "$B@E2,(B" "l22" ,@default)
      ("mie" "$B;0=E(B" "l24" ,@default)
      ("osaka" "$BBg:e(B" "l27" ,@default)
      ("hyogo" "$BJ<8K(B" "l28" ,@default)
      ("kyoto" "$B5~ET(B" "l26" ,@default)
      ("shiga" "$B<"2l(B" "l25" ,@default)
      ("nara" "$BF`NI(B" "l29" ,@default)
      ("wakayama" "$BOB2N;3(B" "l30" ,@default)
      ("tottori" "$BD;<h(B" "l31" ,@default)
      ("shimane" "$BEg:,(B" "l32" ,@default)
      ("okayama" "$B2,;3(B" "l33" ,@default)
      ("hiroshima" "$B9-Eg(B" "l34" ,@default)
      ("yamaguchi" "$B;38}(B" "l35" ,@default)
      ("tokushima" "$BFAEg(B" "l36" ,@default)
      ("kagawa" "$B9a@n(B" "l37" ,@default)
      ("ehime" "$B0&I2(B" "l38" ,@default)
      ("kochi" "$B9bCN(B" "l39" ,@default)
      ("fukuoka" "$BJ!2,(B" "l40" ,@default)
      ("saga" "$B:42l(B" "l41" ,@default)
      ("nagasaki" "$BD9:j(B" "l42" ,@default)
      ("kumamoto" "$B7'K\(B" "l43" ,@default)
      ("oita" "$BBgJ,(B" "l44" ,@default)
      ("miyazaki" "$B5\:j(B" "l45" ,@default)
      ("kagoshima" "$B</;yEg(B" "l46" ,@default)
      ("okinawa" "$B2-Fl(B" "oki" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where numbers point to the search result in order
of [0]url, [1]serial number, [2]year, [3]month, [4]day, [5]subject,
\[6]news source, [7]hour, [8]minute, and [9]news source (the last one
may not be presented).")

(defvar shimbun-yahoo-groups
  (mapcar 'car shimbun-yahoo-groups-table))

(defvar shimbun-yahoo-from-address "nobody@example.com")
(defvar shimbun-yahoo-content-start
  "[012]?[0-9]$B;~(B[0-5]?[0-9]$BJ,G[?.(B[\t\n\r ]*\\(?:</[^>]+>[\t\n\r ]*\\)*")

(defvar shimbun-yahoo-content-end "[\t\n\r ]*\\(<br>[\t\n\r ]*\\)*\
<!-+[\t\n\r ]*interest_match_relevant_zone_end[\t\n\r ]*-+>")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
\(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
;;;<DEBUG>
;;  (shimbun-yahoo-index-url shimbun))
;;
;;(defun shimbun-yahoo-index-url (shimbun)
;;;</DEBUG>
  (let ((group (shimbun-current-group-internal shimbun))
	(url (shimbun-url-internal shimbun)))
    (if (string-equal group "news")
	(concat url "hl")
      (format "%shl?c=%s&t=l"
	      url
	      (nth 2 (assoc group shimbun-yahoo-groups-table))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-yahoo-get-headers shimbun range))
;;
;;(defun shimbun-yahoo-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (from "Yahoo!$B%K%e!<%9(B")
	 (group (shimbun-current-group-internal shimbun))
	 (numbers (cdr (assoc group shimbun-yahoo-groups-table)))
	 (jname (pop numbers))
	 (regexp (progn (setq numbers (cdr numbers)) (pop numbers)))
	 (pages (shimbun-header-index-pages range))
	 (count 0)
	 (index (shimbun-index-url shimbun))
	 id headers start)
    (catch 'stop
      (while t
	(if (string-equal group "news")
	    (progn
	      (when (and (re-search-forward
			  "<!-+[\t\n ]*main[\t\n ]+start[\t\n ]*-+>" nil t)
			 (progn
			   (setq start (match-end 0))
			   (re-search-forward
			    "<!-+[\t\n ]*main[\t\n ]+end[\t\n ]*-+>" nil t)))
		(delete-region (match-beginning 0) (point-max))
		(delete-region (point-min) start)
		(goto-char (point-min)))
	      (when (and (re-search-forward ">[\t\n ]*$B<L??%K%e!<%9(B[\t\n ]*</a>\
\\(?:[\t\n ]*</[^>]+>\\)+[\y\n ]*\\(<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
class=\"ymuiContainer\"\\)" nil t)
			 (shimbun-end-of-tag "div"))
		(delete-region (match-beginning 0) (match-end 0))))
	  (shimbun-remove-tags "<!-+[\t\n ]*$B%"%/%;%9%i%s%-%s%0(B[\t\n ]*-+>"
			       "<!-+[\t\n ]*/$B%"%/%;%9%i%s%-%s%0(B[\t\n ]*-+>"))
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (setq id (concat "<"
			   (save-match-data
			     (shimbun-replace-in-string
			      (match-string (nth 1 numbers))
			      "-" "."))
			   "%" group ".headlines.yahoo.co.jp>"))
	  (unless (and (shimbun-search-id shimbun id)
		       (if (and (>= count 1) ;; We're in the next page.
				;; Stop fetching iff range is not specified.
				(not pages))
			   (throw 'stop nil)
			 t))
	    (if (save-match-data
		  (string-match "$B5-;vA4J8(B[\t\n ]*\\'"
				(match-string (nth 5 numbers))))
		(goto-char (match-end (nth 5 numbers)))
	      (push (shimbun-create-header
		     0
		     (match-string (nth 5 numbers))
		     (concat from " (" jname "/"
			     (or (match-string (nth 6 numbers))
				 (match-string (nth 9 numbers)))
			     ")")
		     (shimbun-make-date-string
		      (string-to-number (match-string (nth 2 numbers)))
		      (string-to-number (match-string (nth 3 numbers)))
		      (string-to-number (match-string (nth 4 numbers)))
		      (format
		       "%02d:%02d"
		       (string-to-number (match-string (nth 7 numbers)))
		       (string-to-number (match-string (nth 8 numbers)))))
		     id "" 0 0
		     (match-string (nth 0 numbers)))
		    headers))))
	(setq count (1+ count))
	(goto-char (point-min))
	(cond ((and pages (>= count pages))
	       (throw 'stop nil))
	      ((string-equal group "news")
	       (if (>= count 2)
		   (throw 'stop nil)
		 (erase-buffer)
		 (shimbun-retrieve-url
		  "http://headlines.yahoo.co.jp/hl?c=flash"
		  t)))
	      ((re-search-forward "<a href=\"\\([^\"]+\\)\">$B<!$N%Z!<%8(B</a>"
				  nil t)
	       (shimbun-retrieve-url (prog1
					 (match-string 1)
				       (erase-buffer))
				     t))
	      ((and (re-search-forward "<!-+[\t\n ]*$B2a5n5-;v(B[\t\n ]*-+>"
				       nil t)
		    (progn
		      (setq start (match-end 0))
		      (re-search-forward "<!-+[\t\n ]*/$B2a5n5-;v(B[\t\n ]*-+>"
					 nil t))
		    (progn
		      (narrow-to-region start (match-beginning 0))
		      (goto-char start)
		      (or (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]+selected[\t\n ]*>"
					     nil t)
			  (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]*>"
					     nil t)))
		    (re-search-forward "<option[\t\n ]+value=\"\
\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\"[\t\n ]*>"
				       nil t))
	       (shimbun-retrieve-url (prog1
					 (concat index "&d=" (match-string 1))
				       (erase-buffer))
				     t))
	      (t
	       (throw 'stop nil)))))
    (shimbun-sort-headers headers)))

(provide 'sb-yahoo)

;;; sb-yahoo.el ends here
