;;; sb-yomiuri.el --- shimbun backend for www.yomiuri.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008 Authors

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>

;; Keywords: news

;;; Copyright:

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

(eval-when-compile (require 'cl)) ;; caddr, cadddr, cddddr.

(require 'shimbun)

(luna-define-class shimbun-yomiuri (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-yomiuri-prefer-text-plain t
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-yomiuri-top-level-domain "yomiuri.co.jp"
  "Name of the top level domain for the Yomiuri On-line.")

(defvar shimbun-yomiuri-url
  (concat "http://www." shimbun-yomiuri-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-yomiuri-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/news/"
	     ;; 2. serial number[1]
	     "\\("
	     ;; 3. year
	     "\\(20[0-9][0-9]\\)"
	     "[01][0-9][0-3][0-9]\\)"
	     ;; 4. serial number[2]
	     "\\([^.]+\\)"
	     "[^\"]+\\)"
	     "\"[^>]*>" s0
	     ;; 5. subject
	     "\\([^<]+\\)"
	     s0 "</a>[^<$B!J(B]*$B!J(B" s0
	     ;; 6. month
	     "\\([01]?[0-9]\\)"
	     s0 "$B7n(B" s0
	     ;; 7. day
	     "\\([0-3]?[0-9]\\)"
	     s0 "$BF|(B" s0
	     ;; 8. hour:minute
	     "\\([012][0-9]:[0-5][0-9]\\)")
	    1 2 4 3 5 6 7 8))
	 (default2
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/"
	     ;; 2. serial number[1]
	     "\\("
	     ;; 3. year
	     "\\(20[0-9][0-9]\\)"
	     "[01][0-9][0-3][0-9]\\)"
	     ;; 4. serial number[2]
	     "\\([^.]+\\)"
	     "[^\"]+\\)"
	     "\"[^>]*>" s0
	     ;; 5. subject
	     "\\([^<]+\\)"
	     s0 "</a>" s0 "<span" s1 "class=\"date\">"
	     "\\(?:" s0 "<span" s1 "[^>]+>[^<]+</span>\\)?"
	     s0 "<span" s1 "class=\"m\">" s0
	     ;; 6. month
	     "\\([01]?[0-9]\\)"
	     s0 "$B7n(B" s0 "</span>" s0 "<span" s1 "class=\"d\">" s0
	     ;; 7. day
	     "\\([0-3]?[0-9]\\)"
	     s0 "$BF|(B" s0 "</span>")
	    1 2 4 3 5 6 7))
	 (kyoiku
	  (cons (format (car default2) "kyoiku\\(?:/[^\"./]+\\)+")
		(cdr default2))))
    `(("atmoney" "$B%^%M!<!&7P:Q(B")
      ("editorial" "$B<R@b!&%3%i%`(B" ""
       ,(concat "<a" s1 "href=\"/"
		;; 1. url
		"\\(%s/news/"
		;; 2. serial number[1]
		"\\("
		;; 3. year
		"\\(20[0-9][0-9]\\)"
		"[01][0-9][0-3][0-9]\\)"
		;; 4. serial number[2]
		"\\([^.]+\\)"
		"[^\"]+\\)"
		"\"[^>]*>" s0
		;; 5. month
		;; 6. ja month
		"\\(?:\\([01]?[0-9]\\)\\|\\([$B#0#1(B]?[$B#0(B-$B#9(B]\\)\\)"
		"$B7n(B"
		;; 7. day
		;; 8. ja day
		"\\(?:\\([0-3]?[0-9]\\)\\|\\([$B#0(B-$B#3(B]?[$B#0(B-$B#9(B]\\)\\)"
		"$BF|IU(B[\t\n $B!!!&(B]*"
		;; 9. subject
		"\\([^<]+\\)"
		s0)
       1 2 4 3 9 5 7 nil 6 8)
      ("entertainment" "$B%(%s%?!<%F%$%s%a%s%H(B")
      ("iryou" "$B0eNE$H2p8n(B" "news/"
       ,(concat ">" s0
		;; 1. ja genre
		"\\(\\cj+\\)"
		"[^<]*</a>[^<]*<a" s1 "href=\"/"
		;; 2. url
		"\\(%s/news/"
		;; 3. genre
		"\\([^_]+\\)"
		"_news/"
		;; 4. serial number[1]
		"\\("
		;; 5. year
		"\\(20[0-9][0-9]\\)"
		;; 6. month
		"\\([01][0-9]\\)"
		;; 7. day
		"\\([0-3][0-9]\\)"
		"\\)"
		;; 8. serial number[2]
		"\\([^.]+\\)"
		"[^\"]+\\)"
		"\"[^>]*>" s0
		;; 9. subject
		"\\([^<]+\\)"
		s0)
       2 4 8 5 9 6 7 nil nil nil 3 1)
      ("kyoiku" "$B650i(B" "" ,@kyoiku)
      ("kyoiku.children" "$B$3$I$b(B")
      ("kyoiku.english" "$B1Q8l(B"
       "http://www.yomiuri.co.jp/kyoiku/learning/english/"
       ,@kyoiku)
      ("kyoiku.qanda" "$B650i(BQ&A")
      ("kyoiku.renaissance" "$B650i%k%M%5%s%9(B"
       "http://www.yomiuri.co.jp/kyoiku/renai/")
      ("kyoiku.special" "$BFC=8(B")
      ("national" "$B<R2q(B" "" ,@default)
      ("politics" "$B@/<#(B" "" ,@default)
      ("science" "$B2J3X(B" "" ,@default)
      ("sports" "$B%9%]!<%D(B" ""
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(%s/"
	 ;; 2. genre
	 "\\([^/]+\\)"
	 "/news/"
	 ;; 3. serial number[1]
	 "\\("
	 ;; 4. year
	 "\\(20[0-9][0-9]\\)"
	 "[01][0-9][0-3][0-9]\\)"
	 ;; 5. serial number[2]
	 "\\([^.]+\\)"
	 "[^\"]+\\)"
	 "\"[^>]*>" s0
	 ;; 6. subject
	 "\\([^<]+\\)"
	 "\\(?:" s0 "<img" s0 "[^>]+>\\)?" s0 "</a>[^<$B!J(B]*$B!J(B" s0
	 ;; 7. month
	 "\\([01]?[0-9]\\)"
	 s0 "$B7n(B" s0
	 ;; 8. day
	 "\\([0-3]?[0-9]\\)"
	 s0 "$BF|(B" s0
	 ;; 9. hour:minute
	 "\\([012][0-9]:[0-5][0-9]\\)")
       1 3 5 4 6 7 8 9 nil nil 2)
      ("world" "$B9q:](B" "" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.
Regexp may contain the \"%s\" token which is replaced with a
regexp-quoted group name.  Numbers point to the search result in order
of [0]url, [1,2]serial numbers, [3]year, [4]subject, [5]month, [6]day,
\[7]hour:minute, [8]ja month, [9]ja day, [10]genre and [11]ja genre.")

(defvar shimbun-yomiuri-subgroups-alist
  (let* ((s0 "[\t\n $B!!(B]*")
	 (s1 "[\t\n ]+")
	 (default
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/"
	     ;; 2. serial number[1]
	     "\\("
	     ;; 3. year
	     "\\(20[0-9][0-9]\\)"
	     "[01][0-9][0-3][0-9]\\)"
	     ;; 4. serial number[2]
	     "\\([^.]+\\)"
	     "[^\"]+\\)"
	     "\"[^>]*>" s0
	     ;; 5. subject
	     "\\([^<]+\\)"
	     s0 "</a>[^<$B!J(B]*$B!J(B" s0
	     ;; 6. month
	     "\\([01]?[0-9]\\)"
	     s0 "$B7n(B" s0
	     ;; 7. day
	     "\\([0-3]?[0-9]\\)"
	     s0 "$BF|(B\\(?:" s0
	     ;; 8. hour:minute
	     "\\([012][0-9]:[0-5][0-9]\\)\\)?")
	    1 2 4 3 5 6 7 8))
	 (default2
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/"
	     ;; 2. serial number[1]
	     "\\("
	     ;; 3. year
	     "\\(20[0-9][0-9]\\)"
	     ;; 4. month
	     "\\([01][0-9]\\)"
	     ;; 5. day
	     "\\([0-3][0-9]\\)"
	     "\\)"
	     ;; 6. serial number[2]
	     "\\([^.]+\\)"
	     "[^\"]+\\)"
	     "\"[^>]*>" s0
	     ;; 7. subject
	     "\\([^<]+\\)"
	     s0)
	    1 2 6 3 7 4 5))
	 (default3
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/"
	     ;; 2. genre
	     "\\([^/]+\\)"
	     "/"
	     ;; 3. serial number[1]
	     "\\("
	     ;; 4. year
	     "\\(20[0-9][0-9]\\)"
	     ;; 5. month
	     "\\([01][0-9]\\)"
	     ;; 6. day
	     "\\([0-3][0-9]\\)"
	     "\\)"
	     ;; 7. serial number[2]
	     "\\([^.]+\\)"
	     "[^\"]+\\)"
	     "\"[^>]*>" s0
	     ;; 8. subject
	     "\\([^<]+\\)"
	     s0)
	    1 3 7 4 8 5 6 nil nil nil 2))
	 (default4
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/"
	     ;; 2. serial number[1]
	     "\\("
	     ;; 3. year
	     "\\(20[0-9][0-9]\\)"
	     "[01][0-9][0-3][0-9]\\)"
	     ;; 4. serial number[2]
	     "\\([^.]+\\)"
	     "[^\"]+\\)"
	     "\"[^>]*>" s0
	     ;; 5. subject
	     "\\([^<]+\\)"
	     s0 "</a>" s0 "<span" s1 "class=\"date\">"
	     "\\(?:" s0 "<span" s1 "[^>]+>[^<]+</span>\\)?"
	     s0 "<span" s1 "class=\"m\">" s0
	     ;; 6. month
	     "\\([01]?[0-9]\\)"
	     s0 "$B7n(B" s0 "</span>" s0 "<span" s1 "class=\"d\">" s0
	     ;; 7. day
	     "\\([0-3]?[0-9]\\)"
	     s0 "$BF|(B" s0 "</span>")
	    1 2 4 3 5 6 7))
	 (entertainment
	  (list
	   (concat
	    "alt=\""
	    ;; 1. ja genre
	    "\\([^\"]+\\)"
	    "\"[^>]+>" s0 "</a>\\(?:" s0 "</[^>]+>\\)?[^<]*<a" s1 "href=\"/"
	    ;; 2. url
	    "\\(entertainment/%s/"
	    ;; 3. genre
	    "\\([^/]+\\)"
	    "/"
	    ;; 4. serial number[1]
	    "\\("
	    ;; 5. year
	    "\\(20[0-9][0-9]\\)"
	    "[01][0-9][0-3][0-9]\\)"
	    ;; 6. serial number[2]
	    "\\([^.]+\\)"
	    "[^\"]+\\)"
	    "\"[^>]*>" s0
	    ;; 7. subject
	    "\\([^<]+\\)"
	    s0 "</a>[^<$B!J(B]*$B!J(B" s0
	    ;; 8. month
	    "\\([01]?[0-9]\\)"
	    s0 "$B7n(B" s0
	    ;; 9. day
	    "\\([0-3]?[0-9]\\)"
	    s0 "$BF|(B")
	   2 4 6 5 7 8 9 nil nil nil 3 1)))
    `(("atmoney"
       ("$BJu$/$8(B" "lottery" "http://www.yomiuri.co.jp/atmoney/lottery/"
	,(format (car default2) "atmoney/lottery") ,@(cdr default2))
       ("$B6bM;%K%e!<%9(B" "mnews" "http://www.yomiuri.co.jp/atmoney/mnews/"
	,(format (car default) "atmoney/mnews") ,@(cdr default))
       ("$B7P:Q%K%e!<%9(B" "news" "http://www.yomiuri.co.jp/atmoney/news/"
	,(format (car default) "atmoney/news") ,@(cdr default))
       ("$B?7@=IJ>pJs(B" "pnews" "http://www.yomiuri.co.jp/atmoney/pnews/"
	,(format (car default) "atmoney/pnews") ,@(cdr default)))
      ("entertainment"
       ("$B1G2h(B" nil "http://www.yomiuri.co.jp/entertainment/cinema/"
	,(format (car entertainment) "cinema") ,@(cdr entertainment))
       ("$B#d#o#n#n#a(B" "donna" "http://www.yomiuri.co.jp/donna/"
	,(concat
	  "<a" s1 "href=\"/"
	  ;; 1. url
	  "\\(donna/"
	  ;; 2. serial number[1]
	  "\\(do\\)"
	  "_"
	  ;; 3. serial number[2]
	  "\\("
	  ;; 4. year
	  "\\([0-9][0-9]\\)"
	  ;; 5. month
	  "\\([01][0-9]\\)"
	  ;; 6. day
	  "\\([0-3][0-9]\\)"
	  "\\)"
	  "[^\"]+\\)"
	  "[^>]*>" s0
	  ;; 7. subject
	  "\\([^<]+\\)")
	1 2 3 4 7 5 6)
       ("DVD$B>pJs(B" "dvd" "http://www.yomiuri.co.jp/entertainment/cinema/dvd/"
	,(format (car default) "entertainment/cinema/dvd") ,@(cdr default))
       ("$B%8%V%j$r$$$C$Q$$(B" "ghibli"
	"http://www.yomiuri.co.jp/entertainment/ghibli/"
	,(concat
	  "<a" s1 "href=\""
	  ;; 1. url
	  "\\(cnt_"
	  ;; 2. genre
	  "\\([^_]+\\)"
	  "_"
	  ;; 3. serial number[1]
	  "\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)"
	  ;; 4. serial number[2]
	  "\\([^.]+\\)"
	  "[^\"]+\\)"
	  "[^>]*>" s0
	  ;; 5. subject
	  "\\([^<]+\\)"
	  "</a>" s0 "$B!J(B"
	  ;; 6. year
	  "\\(20[0-9][0-9]\\)" "$BG/(B"
	  ;; 7. month
	  "\\([01]?[0-9]\\)" "$B7n(B"
	  ;; 8. day
	  "\\([0-3]?[0-9]\\)" "$BF|(B")
	1 3 4 6 5 7 8 nil nil nil 2)
       ("$B%X%6!<$N1G2h4[(B" "heather"
	"http://www.yomiuri.co.jp/entertainment/heather/"
	,(format (car entertainment) "heather") ,@(cdr entertainment))
       ("$B2;3Z(B" "music" "http://www.yomiuri.co.jp/entertainment/music/"
	,(format (car default3) "entertainment/music") ,@(cdr default3))
       ("$B%K%e!<%9(B" "news" "http://www.yomiuri.co.jp/entertainment/news/"
	,(format (car default) "entertainment/news") ,@(cdr default))
       ("$BIqBf(B" "stage" "http://www.yomiuri.co.jp/entertainment/stage/"
	,(format (car entertainment) "stage") ,@(cdr entertainment))
       ("$B#T#V(B" "tv" "http://www.yomiuri.co.jp/entertainment/tv/"
	,(format (car default) "entertainment/tv") ,@(cdr default))
       ("$B#Y!u#Y%F%l%S(B" "yy" "http://www.yomiuri.co.jp/entertainment/yy/"
	,(format (car default3) "entertainment/yy") ,@(cdr default3)))
      ("kyoiku"
       ("$B%K%e!<%9(B" nil "http://www.yomiuri.co.jp/kyoiku/news/"
	,(format (car default4) "kyoiku/news") ,@(cdr default4))
       ("$B650i9T@/(B" nil "http://www.yomiuri.co.jp/kyoiku/news2/06.htm"
	,(format (car default4) "kyoiku/news2") ,@(cdr default4))
       ("$B%\%i%s%F%#%"!&$=$NB>(B" nil
	"http://www.yomiuri.co.jp/kyoiku/news2/08.htm"
	,(format (car default4) "kyoiku/news2") ,@(cdr default4)))
      ("kyoiku.children"
       ("$B#J%-%C%:DL?.(B" "jkids"
	"http://www.yomiuri.co.jp/kyoiku/children/jkids/"
	,(format (car default4) "kyoiku/children/jkids") ,@(cdr default4))
       ("$B;R$I$b$N?4(B" "kodomo.hagukumu"
	"http://www.yomiuri.co.jp/kyoiku/hagukumu/kodomo/"
	,(format (car default4) "kyoiku/hagukumu/kodomo") ,@(cdr default4))
       ("$B%K%e!<%9%&%#!<%/%j!<(B" "weekly"
	"http://www.yomiuri.co.jp/kyoiku/children/weekly/"
	,(format (car default4) "kyoiku/children/weekly") ,@(cdr default4)))
      ("kyoiku.qanda"
       ("$B650iAjCL(B" "consul" "http://www.yomiuri.co.jp/kyoiku/qanda/consul/"
	,(format (car default4) "kyoiku/qanda/consul") ,@(cdr default4))
       ("$BG:$_$N$A@2$l(B" "worries"
	"http://www.yomiuri.co.jp/kyoiku/qanda/worries/"
	,(format (car default4) "kyoiku/qanda/worries") ,@(cdr default4)))
      ("kyoiku.special"
       ("$B<u83#A#B#C(B" "s06" "http://www.yomiuri.co.jp/kyoiku/special/s06/"
	,(format (car default4) "kyoiku/special/s06") ,@(cdr default4)))
      ("national"
       ("$BJ82=(B" "culture" "http://www.yomiuri.co.jp/national/culture/"
	,(format (car default) "national/culture/news") ,@(cdr default))
       ("$B$*$/$d$_(B" "obit" "http://www.yomiuri.co.jp/national/obit/"
	,(format (car default2) "national/obit/news") ,@(cdr default2)))
      ("sports"
       ("$B%(%H%;%H%i(B" "etc" "http://www.yomiuri.co.jp/sports/etc/"
	,(format (car default) "sports/etc/news") ,@(cdr default))
       ("$B%4%k%U(B" "golf" "http://www.yomiuri.co.jp/sports/golf/"
	,(format (car default) "sports/golf/news") ,@(cdr default))
       ("$BBg%j!<%0(B" "mlb" "http://www.yomiuri.co.jp/sports/mlb/"
	,(format (car default) "sports/mlb/news") ,@(cdr default))
       ("$B%W%mLn5e(B" "npb" "http://www.yomiuri.co.jp/sports/npb/"
	,(format (car default) "sports/npb/news") ,@(cdr default))
       ("$B%5%C%+!<(B" "soccer" "http://www.yomiuri.co.jp/sports/soccer/"
	,(format (car default) "sports/soccer/news") ,@(cdr default))
       ("$BEl5~O;Bg3XLn5e(B07" "ubb07" "http://www.yomiuri.co.jp/sports/ubb07/"
	,(format (car default) "sports/ubb07/news") ,@(cdr default)))))
  "Alist of parent groups and lists of subgenres and tables for subgroups.
Each table is the same as the `cdr' of the element of
`shimbun-yomiuri-group-table'.")

(defvar shimbun-yomiuri-content-start
  "\n<!--// contents_area_start //-->\n\
\\|\n<!--// article_start //-->\n\
\\|\n<!-- $B"'<L??%F!<%V%k"'(B -->\n\
\\|\n<!--  honbun start  -->\n")

(defvar shimbun-yomiuri-content-end
  "\n<!--// contents_area_end //-->\n\
\\|\n<!--// article_end //-->\n\
\\|\n<!--  honbun end  -->\n")

(defvar shimbun-yomiuri-text-content-start
  "\n<!--// contents_area_start //-->\n\
\\|\n<!--// article_start //-->\n\
\\|\n<!--  honbun start  -->\n")

(defvar shimbun-yomiuri-text-content-end shimbun-yomiuri-content-end)

(defvar shimbun-yomiuri-x-face-alist
  '(("default" . "X-Face: #sUhc'&(fVr$~<rt#?PkH,u-.fV(>y)\
i\"#,TNF|j.dEh2dAzfa4=IH&llI]S<-\"dznMW2_j\n [N1a%n{SU&E&\
Ex;xlc)9`]D07rPEsbgyjP@\"_@g-kw!~TJNilrSC!<D|<m=%Uf2:eebg")))

(defvar shimbun-yomiuri-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-yomiuri)
						&rest init-args)
  (shimbun-set-server-name-internal shimbun "$Bl&Gd?7J9(B")
  (shimbun-set-from-address-internal shimbun "nobody@example.com")
  ;; To share class variables between `shimbun-yomiuri' and its
  ;; successor `shimbun-yomiuri-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-yomiuri-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-yomiuri-expiration-days)
  (shimbun-set-content-start-internal shimbun shimbun-yomiuri-content-start)
  (shimbun-set-content-end-internal shimbun shimbun-yomiuri-content-end)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-yomiuri))
  (mapcar 'car shimbun-yomiuri-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-yomiuri))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-yomiuri-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-yomiuri))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (nth 2 (assoc group shimbun-yomiuri-group-table))))
    (cond ((not index)
	   "about:blank")
	  ((string-match "\\`http:" index)
	   index)
	  (t
	   (concat shimbun-yomiuri-url group "/" index)))))

(defun shimbun-yomiuri-japanese-string-to-number (string)
  "Convert a Japanese zenkaku number to just a number."
  (let ((alist '((?$B#0(B . 0) (?$B#1(B . 1) (?$B#2(B . 2) (?$B#3(B . 3) (?$B#4(B . 4)
		 (?$B#5(B . 5) (?$B#6(B . 6) (?$B#7(B . 7) (?$B#8(B . 8) (?$B#9(B . 9)))
	(len (length string))
	(idx 0)
	(num 0))
    (while (< idx len)
      (setq num (+ (cdr (assq (aref string idx) alist)) (* num 10))
	    idx (1+ idx)))
    num))

(defun shimbun-yomiuri-get-top-header (group from shimbun)
  "Return a list of a header for the top news."
  (when (and (search-forward "<!--// top_news_start -->" nil t)
	     (re-search-forward
	      (format
	       (eval-when-compile
		 (let ((s0 "[\t\n ]*")
		       (s1 "[\t\n ]+"))
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/"
		    ;; 2. genre
		    "\\(?:\\([^/]+\\)/\\)?"
		    "news/"
		    ;; 3. serial number[1]
		    "\\("
		    ;; 4. year
		    "\\(20[0-9][0-9]\\)"
		    "[01][0-9][0-3][0-9]\\)"
		    ;; 5. serial number[2]
		    "\\([^.]+\\)"
		    "[^\"]+\\)"
		    "\"[^>]*>" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0)))
	       group)
	      nil t))
    (let* ((url (shimbun-expand-url (match-string 1) shimbun-yomiuri-url))
	   (genre (match-string 2))
	   (id (concat "<" (match-string 3) "." (match-string 5)
		       "%" (when genre
			     (concat genre "."))
		       group "." shimbun-yomiuri-top-level-domain ">"))
	   year subject)
      (prog1
	  (unless (shimbun-search-id shimbun id)
	    (setq year (string-to-number (match-string 4))
		  subject (match-string 6))
	    (when (re-search-forward
		   (eval-when-compile
		     (let ((s0 "[\t\n ]*"))
		       (concat
			">" s0 "$B!J(B" s0
			;; 1. month
			"\\([01]?[0-9]\\)"
			s0 "$B7n(B" s0
			;; 2. day
			"\\([0-3]?[0-9]\\)"
			s0 "$BF|(B" s0
			;; 3. hour:minute
			"\\([012][0-9]:[0-5][0-9]\\)"
			s0 "$B!K(B" s0 "<")))
		   nil t)
	      (list (shimbun-create-header
		     0
		     subject
		     from
		     (shimbun-make-date-string
		      year
		      (string-to-number (match-string 1))
		      (string-to-number (match-string 2))
		      (match-string 3))
		     id "" 0 0 url))))
	(search-forward "<!--// top_news_end //-->" nil t)))))

(defun shimbun-yomiuri-get-headers (shimbun)
  "Return a list of headers."
  (let* ((group (shimbun-current-group-internal shimbun))
	 (from (concat (shimbun-server-name shimbun)
		       " (" (shimbun-current-group-name shimbun) ")"))
	 (case-fold-search t)
	 (regexp (assoc group shimbun-yomiuri-group-table))
	 (subgroups (cdr (assoc group shimbun-yomiuri-subgroups-alist)))
	 numbers headers subject month day genre subgenre jgenre id year
	 subgrp)
    (setq regexp
	  (when (setq numbers (nthcdr 4 regexp))
	    (format (nth 3 regexp) (regexp-quote group))))
    (catch 'stop
      ;; The loop for fetching all the articles in the subgroups.
      (while t
	(shimbun-strip-cr)
	(goto-char (point-min))
	;; Extract top news.
	(when (member group '("atmoney" "entertainment" "national" "politics"
			      "science" "sports" "world"))
	  (setq headers
		(nconc headers
		       (shimbun-yomiuri-get-top-header group from shimbun))))
	(when regexp
	  (while (re-search-forward regexp nil t)
	    (setq subject (match-string (nth 4 numbers))
		  month (if (and (nth 8 numbers)
				 (match-beginning (nth 8 numbers)))
			    (shimbun-yomiuri-japanese-string-to-number
			     (match-string (nth 8 numbers)))
			  (string-to-number (match-string (nth 5 numbers))))
		  day (if (and (nth 9 numbers)
			       (match-beginning (nth 9 numbers)))
			  (shimbun-yomiuri-japanese-string-to-number
			   (match-string (nth 9 numbers)))
			(string-to-number (match-string (nth 6 numbers))))
		  genre (or subgenre
			    (when (nth 10 numbers)
			      (match-string (nth 10 numbers))))
		  jgenre (when (nth 11 numbers)
			   (match-string (nth 11 numbers))))
	    (cond ((string-equal group "editorial")
		   (setq subject
			 (format
			  "%02d/%02d %s"
			  month day
			  (save-match-data
			    (if (string-match "\\`$B!N(B\\(.+\\)$B!O!V(B\\(.+\\)$B!W(B\\'"
					      subject)
				(replace-match "\\1: \\2" nil nil subject)
			      subject)))))
		  (jgenre
		   (setq subject (concat "[" jgenre "] " subject))))
	    (setq id (concat "<" (match-string (nth 1 numbers))
			     "." (match-string (nth 2 numbers))
			     "%" (when genre
				   (concat genre "."))
			     (mapconcat
			      'identity
			      (nreverse (save-match-data
					  (split-string group "\\.")))
			      ".")
			     "." shimbun-yomiuri-top-level-domain ">"))
	    (unless (shimbun-search-id shimbun id)
	      (when (< (setq year (string-to-number
				   (match-string (nth 3 numbers))))
		       100)
		(setq year (+ year 2000)))
	      (push (shimbun-create-header
		     0 subject from
		     (shimbun-make-date-string
		      year month day
		      (when (and (nth 7 numbers)
				 (match-beginning (nth 7 numbers)))
			(match-string (nth 7 numbers))))
		     id "" 0 0
		     (shimbun-expand-url
		      (match-string (nth 0 numbers))
		      (if (string-match "$B%8%V%j$r$$$C$Q$$(B" from)
			  "http://www.yomiuri.co.jp/entertainment/ghibli/"
			shimbun-yomiuri-url)))
		    headers))))
	(if subgroups
	    (progn
	      (erase-buffer)
	      (setq subgrp (pop subgroups)
		    from (concat (shimbun-server-name shimbun)
				 " (" (car subgrp) ")")
		    subgenre (cadr subgrp))
	      (shimbun-retrieve-url (caddr subgrp))
	      (setq regexp (cadddr subgrp)
		    numbers (cddddr subgrp)))
	  (throw 'stop nil))))
    (shimbun-sort-headers headers)))

(defun shimbun-yomiuri-get-headers-kyoiku-renaissance (shimbun)
  (let ((from (concat (shimbun-server-name shimbun)
		      " (%s/" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	next genre start end id headers)
    (while (cond ((eq next 'none)
		  nil)
		 (next
		  (set-match-data next)
		  (goto-char (match-end 0)))
		 (t
		  (re-search-forward
		   "<h3[\t\n ]+class=\"hdst\">[\t\n ]*\\([^<]+\\)[\t\n ]*</h3>"
		   nil t)))
      (setq genre (match-string 1)
	    start (match-end 0))
      (if (re-search-forward
	   "<h3[\t\n ]+class=\"hdst\">[\t\n ]*\\([^<]+\\)[\t\n ]*</h3>"
	   nil t)
	  (setq end (match-beginning 0)
		next (match-data))
	(setq end nil
	      next 'none))
      (goto-char start)
      (while (re-search-forward "\
<a[\t\n ]+href=\"/\\(kyoiku/renai/\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\
\\([^.]+\\)[^\"]+\\)\"[^>]*>[\t\n ]*\\([^<]+\\)[\t\n ]*</a>[\t\n ]*\
<span[\t\n ]+class=\"date\">[\t\n ]*<span[\t\n ]+class=\"y\">[\t\n ]*\
\\(20[0-9][0-9]\\)[\t\n ]*$BG/(B[\t\n ]*</span>[\t\n ]*<span[\t\n ]+class=\"m\">\
\[\t\n ]*\\([01]?[0-9]\\)[\t\n ]*$B7n(B[\t\n ]*</span>[\t\n ]*\
<span[\t\n ]+class=\"d\">[\t\n ]*\\([0-3]?[0-9]\\)[\t\n ]*$BF|(B[\t\n ]*</span>"
				end t)
	(setq id (concat "<" (match-string 2) "." (match-string 3)
			 "%renai.kyoiku." shimbun-yomiuri-top-level-domain
			 ">"))
	(unless (shimbun-search-id shimbun id)
	  (push (shimbun-create-header
		 0 (match-string 4) (format from genre)
		 (shimbun-make-date-string
		  (string-to-number (match-string 5))
		  (string-to-number (match-string 6))
		  (string-to-number (match-string 7)))
		 id "" 0 0
		 (shimbun-expand-url (match-string 1) shimbun-yomiuri-url))
		headers))))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yomiuri)
					 &optional range)
  (if (string-equal (shimbun-current-group-internal shimbun)
		    "kyoiku.renaissance")
      (shimbun-yomiuri-get-headers-kyoiku-renaissance shimbun)
    (shimbun-yomiuri-get-headers shimbun)))

(defun shimbun-yomiuri-prepare-article (shimbun header)
  (shimbun-with-narrowed-article
   shimbun
   ;; Correct Date header.
   (when (re-search-forward
	  (eval-when-compile
	    (let ((s0 "[\t\n ]*")
		  (s1 "[\t\n ]+"))
	      (concat
	       "<!--" s0 "//" s1 "date_start" s1 "//" s0 "-->" s0
	       ;; 1. year
	       "\\(20[0-9][0-9]\\)"
	       s0 "$BG/(B" s0
	       ;; 2. month
	       "\\([01]?[0-9]\\)"
	       s0 "$B7n(B" s0
	       ;; 3. day
	       "\\([0-3]?[0-9]\\)"
	       s0 "$BF|(B" s0
	       ;; 4. hour
	       "\\([012]?[0-9]\\)"
	       s0 "$B;~(B" s0
	       ;; 5. minute
	       "\\([0-5]?[0-9]\\)"
	       s0 "$BJ,(B" s0 "<!--" s0 "//" s1 "date_end" s1 "//" s0 "-->")))
	  nil t)
     (shimbun-header-set-date
      header
      (shimbun-make-date-string
       (string-to-number (match-string 1))
       (string-to-number (match-string 2))
       (string-to-number (match-string 3))
       (format "%02d:%02d"
	       (string-to-number (match-string 4))
	       (string-to-number (match-string 5)))))
     (goto-char (point-min)))
   ;; Remove the $B%U%)%H%K%e!<%9(B, the $B<L??$N3HBg(B buttons, etc.
   (while (re-search-forward
	   (eval-when-compile
	     (let ((s0 "[\t\n ]*")
		   (s1 "[\t\n ]+")
		   (n1 "[^\t\n >]+"))
	       (concat
		s0
		"\\(?:"
		"<a\\(?:" s1 n1 "\\)*" s1
		"\\(?:class=\"photo-pn\"\\|target=\"photoWin\"\\)"
		"\\(?:" s1 n1 "\\)*" s0 ">"
		"\\|"
		"<img\\(?:" s1 n1 "\\)*" s1
		"\\(?:alt=\"$B%U%)%H%K%e!<%9(B\"\\|class=\"photo-el\"\\)"
		"\\(?:" s1 n1 "\\)*" s0 ">"
		"\\|"
		"<div" s1
		"class=\"enlargedphoto\">\\(?:[^<>]+\\)?"
		"<img" s1 "[^>]+>" s0 "</div>"
		"\\|"
		"<div" s1 "class=\"[^\"]+\">" s0
		"<img\\(?:" s1 n1 "\\)*" s1 "src=\"/g/d\\.gif\""
		"\\(?:" s1 n1 "\\)*" s0 ">" s0 "</div>"
		"\\|"
		s0 "rectangle(\"[^\"]+\");" s0
		"\\)" s0)))
	   nil t)
     (delete-region (match-beginning 0) (match-end 0)))
   (goto-char (point-min))
   ;; Replace $B<L??$N3HBg(B with $B<L??(B.
   (while (re-search-forward
	   (eval-when-compile
	     (let ((s1 "[\t\n ]+")
		   (n1 "[^\t\n >]+"))
	       (concat "<img\\(?:" s1 n1 "\\)*" s1
		       "alt=\"$B<L??(B\\($B$N3HBg(B\\)\"")))
	   nil t)
     (delete-region (match-beginning 1) (match-end 1)))
   ;; Remove javascripts which will appear in text/plain articles.
   (shimbun-remove-tags "<!--// rectangle_start //-->"
			"<!--// rectangle_end //-->")
   (goto-char (point-min))
   (let ((group (shimbun-current-group-internal shimbun)))
     (cond ((string-equal group "editorial")
	    ;; Break continuous lines.
	    (when (string-match " \\(?:$B$h$_$&$j@#I>(B\\|$BJT=8<jD"(B\\)\\'"
				(shimbun-header-subject header 'no-encode))
	      (while (search-forward "$B"!(B" nil t)
		(replace-match "$B!#(B<br><br>\n$B!!(B"))))
	   ((string-equal group "entertainment")
	    ;; Remove trailing garbage.
	    (when (re-search-forward "[\t\n ]*\
<!-+<p[\t\n ]+class=\"align-c\">$B!~(B+</p>-+>[\t\n ]*"
				     nil t)
	      (delete-region (match-beginning 0) (match-end 0)))
	    ;; Remove nav and track-back button, etc.
	    (shimbun-remove-tags
	     "[\t\n ]*<div[\t\n ]+class=\"\\(?:cl\\|nav-bn\\|track-back\\)\">"
	     "</div>[\t\n ]*"))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-yomiuri)
						   header)
  (shimbun-yomiuri-prepare-article shimbun header))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-yomiuri)
						    header)
  (when (luna-call-next-method)
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-yomiuri)

;;; sb-yomiuri.el ends here
