;;; sb-asahi.el --- shimbun backend for asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001-2011 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>,
;;         NOMIYA Masaru      <nomiya@ttmy.ne.jp>
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

(eval-when-compile (require 'cl))

(require 'shimbun)
(require 'sb-multi)

(luna-define-class shimbun-asahi (shimbun-japanese-newspaper shimbun-multi
							     shimbun) ())

(defvar shimbun-asahi-prefer-text-plain t
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-asahi-top-level-domain "asahi.com"
  "Name of the top level domain for the Asahi shimbun.")

(defvar shimbun-asahi-url
  (concat "http://www." shimbun-asahi-top-level-domain "/")
  "Name of the parent url.")

(defun shimbun-asahi-make-regexp (name)
  "Return a list of a regexp and numbers for the group NAME.
Every `.' in NAME will be replaced with `/'."
  (list (let ((s0 "[\t\n $B!!(B]*")
	      (s1 "[\t\n ]+")
	      (no-nl "[^\n<>]+"))
	  (concat "<a" s1 "href=\"/"
		  ;; 1. url
		  "\\(" (shimbun-subst-char-in-string ?. ?/ name) "/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"))
	1 nil 2 6 3 4 5))

(defvar shimbun-asahi-group-table
  (let* ((s0 "[\t\n $B!!(B]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/update/"
		    ;; 2. month
		    "\\([01][0-9]\\)"
		    ;; 3. day
		    "\\([0-3][0-9]\\)"
		    "/"
		    ;; 4. serial number
		    "\\([a-z]*[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 5. subject
		    "\\(" no-nl "\\)"
		    s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		   1 4 nil 5 nil 2 3))
	 (default2 (shimbun-asahi-make-regexp "%s"))
	 (book (list
		(concat
		 "<a" s1 "href=\"/"
		 ;; 1. url
		 "\\(%s/"
		 ;; 2. serial number
		 "\\([a-z]*"
		 ;; 3. year
		 "\\(20[0-9][0-9]\\)"
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 "[0-9]+\\)"
		 "\\.html\\)"
		 "\"" s0 ">" s0
		 ;; 6. subject
		 "\\(" no-nl "\\)"
		 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		1 2 nil 6 3 4 5))
	 (edu (shimbun-asahi-make-regexp "edu.news"))
	 (international (list
			 (concat
			  "<a" s1 "href=\"/"
			  ;; 1. url
			  "\\(international/update/"
			  ;; 2. month
			  "\\([01][0-9]\\)"
			  ;; 3. day
			  "\\([0-3][0-9]\\)"
			  "/"
			  ;; 4. serial number
			  "\\([a-z]*[0-9]+\\)"
			  "\\.html\\)"
			  "\">" s0
			  ;; 5. subject
			  "\\(" no-nl "\\)"
			  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
			 1 4 nil 5 nil 2 3))
	 (rss (list
	       (concat
		"<item" s1 "rdf:about=\""
		;; 1. url
		"\\(http://\\(?:book\\|www\\)\\.asahi\\.com//?"
		;; 2. extra keyword (en)
		"\\([^/]+\\)"
		"\\(?:/update/"
		;; 3 and 4. serial number
		"\\([0-9]+\\)\\)?/\\([a-z]*[0-9]+\\)"
		"\\.html\\?ref=rss\\)"
		"\"" s0 ">" s0 "<title>" s0
		;; 5. subject
		"\\([^<]+\\)"
		s0 "</title>\\(?:"
		s0 "\\(?:<[^>]+/>"
		"\\|<[^>]+>\\(?:<[^]]+\\]+>\\|[^<]+\\)</[^>]+>\\)\\)*"
		s0 "<dc:subject>" s0
		;; 6. extra keyword (ja)
		"\\([^<]+\\)"
		s0 "</dc:subject>" s0 "<dc:date>" s0
		;; 7. year
		"\\(20[0-9][0-9]\\)"
		"-"
		;; 8. month
		"\\([01][0-9]\\)"
		"-"
		;; 9. day
		"\\([0-3][0-9]\\)"
		"T"
		;; 10. hour:min:sec
		"\\([012][0-9]:[0-5][0-9]:[0-5][0-9]\\)")
	       1 3 4 5 7 8 9 10 2 nil 6)))
    `(("book" "RSS" "http://www3.asahi.com/rss/book.rdf" ,@rss)
      ("book.column" "$B%3%i%`(B")
      ("book.news" "$B=PHG%K%e!<%9(B" nil ,@book)
      ("book.paperback" "$BJ88K!&?7=q(B")
      ("book.review" "$B=qI>(B" nil ,@book)
      ("book.special" "$BFC=8(B" nil
       ,(concat
	 "<a" s1 "href=\"\\(?:http://book\\.asahi\\.com\\)?/"
	 ;; 1. url
	 "\\([^/]+/"
	 ;; 2. serial number
	 "\\([a-z]*"
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]+\\)"
	 "\\.html\\)"
	 "\"" s0 ">" s0
	 ;; 6. subject
	 "\\(" no-nl "\\)")
       1 2 nil 6 3 4 5)
      ("business" "$B%S%8%M%9(B" "%s/list.html" ,@default)
      ("car" "$B0&<V(B" "%s/news/" ,@(shimbun-asahi-make-regexp "car.news"))
      ("culture" "$BJ82=!&7]G=(B" "%s/list.html"
       ,(concat "<a" s1 "href=\"/"
		;; 1. url
		"\\(culture/"
		"\\(?:[^/]+/\\)?"
		;; 2. serial number
		"\\([a-z]*"
		;; 3. year
		"\\(20[0-9][0-9]\\)"
		;; 4. month
		"\\([01][0-9]\\)"
		;; 5. day
		"\\([0-3][0-9]\\)"
		"[0-9]+\\)"
		"\\.html\\)"
		"\">" s0
		;; 6. subject
		"\\([^<]+\\)"
		s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0
		"<\\(?:/dt\\|span\\)")
       1 nil 2 6 3 4 5)
      ("digital" "$B%G%8%?%k(B" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "digital/[^\"/]+"))
      ("editorial" "$B<R@b(B" "include/editorial_bno4.xml"
       ,(concat
	 "<a" s1 "href=\"\\./"
	 ;; 1. url
	 "\\(editorial"
	 ;; 2. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 "\\.html\\)"
	 "\"")
       1 nil nil nil 2 3 4)
      ("edu" "$B650i(B" "%s/list.html" ,@edu)
      ("english" "ENGLISH" "%s/index.html"
       ,@(let ((rest (shimbun-asahi-make-regexp "english.Herald-asahi")))
	   (cons (concat
		  (car rest)
		  "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		 (cdr rest))))
      ("food" "$B?)(B" "%s/news/" ,@(shimbun-asahi-make-regexp "food.news"))
      ("health" "$B7r9/(B" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "health.news"))
      ("housing" "$B=;$^$$(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "housing.news"))
      ("igo" "$B0O8k(B" "%s/news/" ,@(shimbun-asahi-make-regexp "igo.news"))
      ("international" "$B9q:](B" "%s/list.html" ,@default)
      ("international.asia" "$B%"%8%"(B" "international/asia.html" ,@international)
      ("international.column" "$B%3%i%`(B")
      ("international.special" "$BFC=8(B")
      ("international.world" "$B@$3&(B")
      ("job" "$B="?&!&E>?&(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "job.news"))
      ("kansai" "$B4X@>(B" "%s/news/" ,@(shimbun-asahi-make-regexp "kansai.news"))
      ("kansai.entertainment" "$B3Z$7$`(B")
      ("kansai.kokoro" "$B$3$3$m(B")
      ("kansai.sumai" "$B=;$^$$(B")
      ("kansai.taberu" "$B?)$Y$k(B")
      ("komimi" "$B%3%_%_8}%3%_(B" "%s/list.html" ,@default2)
      ("life" "$BJk$i$7(B" "%s/list.html" ,@default)
      ("life.column" "$B%3%i%`(B")
      ("national" "$B<R2q(B" "%s/list.html" ,@default)
      ("politics" "$B@/<#(B" "%s/list.html" ,@default)
      ("rss" "RSS" "http://www3.asahi.com/rss/index.rdf" ,@rss)
      ("science" "$B%5%$%(%s%9(B" "%s/list.html" ,@default)
      ("shopping" "$B%7%g%C%T%s%0(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "shopping.news"))
      ("shopping.column" "$B%3%i%`(B")
      ("shopping.yakimono" "$B$d$-$b$N(B")
      ("shougi" "$B>-4}(B")
      ("sports" "$B%9%]!<%D(B" "%s/list.html" ,@default)
      ("sports.baseball" "$BLn5e(B")
      ("sports.battle" "$B3JF.5;!&AjKP(B")
      ("sports.etc" "$B$=$NB>(B")
      ("sports.football" "$B%5%C%+!<(B")
      ("sports.golf" "$B%4%k%U(B")
      ("sports.rugby" "$B%i%0%S!<(B")
      ("sports.usa" "$BJF%W%m%9%]!<%D(B")
      ("sports.winter" "$B%&%$%s%?!<%9%]!<%D(B")
      ("tenjin" "$BE7@<?M8l(B" "include/column_bno4.xml"
       ,(concat
	 "<a" s1 "href=\"\\./"
	 ;; 1. url
	 "\\(column"
	 ;; 2. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 "\\.html\\)"
	 "\"")
       1 nil nil nil 2 3 4)
      ("travel" "$B%H%i%Y%k(B" "%s/news/"
       ,@(shimbun-asahi-make-regexp "travel.news"))
      ("wakata" "$B<cED$5$s$-$\$&BZ:_5-(B" "%s/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(wakata\\(?:/[^\"/]+\\)*/"
	 ;; 2. serial number
	 "\\([a-z]*"
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]+\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 6. subject
	 "\\([^\n<>]+\\)"
	 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)*</a>")
       1 nil 2 6 3 4 5)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
\[4]a year, [5]a month, [6]a day, [7]an hour:minute, [8,9,10]an extra
keyword, [11]hour and [12]minute.  If an index page is nil, a group
name in which \".\" is substituted with \"/\" is used instead.")

(defvar shimbun-asahi-subgroups-alist
  (let* ((s0 "[\t\n $B!!(B]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/update/"
		    ;; 2. month
		    "\\([01][0-9]\\)"
		    ;; 3. day
		    "\\([0-3][0-9]\\)"
		    "/"
		    ;; 4. serial number
		    "\\([a-z]*[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 5. subject
		    "\\(" no-nl "\\)"
		    s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		   1 4 nil 5 nil 2 3))
	 (baseball (shimbun-asahi-make-regexp "sports.bb"))
	 (book1 (list
		 (concat
		  "<a" s1 "href=\"/"
		  ;; 1. url
		  "\\(%s/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\"" s0 ">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		 1 2 nil 6 3 4 5))
	 (book2 (list
		 (concat
		  "<a" s1 "href=\"http://book\\.asahi\\.com/"
		  ;; 1. url
		  "\\([^/]+/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\"" s0 ">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		 1 2 nil 6 3 4 5))
	 (business (list
		    (concat
		     "<a" s1 "href=\""
		     ;; 1. url
		     "\\(/business/%s/\\(?:[^/]+/\\)?"
		     ;; 2. serial number
		     "\\([a-z]+"
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "\\(?:[0-9]+\\)"
		     "\\.html\\)\\)"
		     "\"" s0 ">" s0
		     ;; 6. subject
		     "\\(" no-nl "\\)"
		     "</a><span class=\"")
		    1 2 nil 6 3 4 5))
	 (culture (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(culture\\(?:/[^\"/]+\\)+/"
		    ;; 2. serial number
		    "\\([a-z]*"
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 6. subject
		    "\\(" no-nl "\\)"
		    s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"
		    "[\t\n ]*\\(?:</dt>\\|<span[\t\n ]+\\)")
		   1 nil 2 6 3 4 5))
	 (edu (nthcdr 3 (assoc "edu" shimbun-asahi-group-table)))
	 (football (shimbun-asahi-make-regexp "sports.fb"))
	 (health (nthcdr 3 (assoc "health" shimbun-asahi-group-table)))
	 (international (nthcdr 3 (assoc "international.asia"
					 shimbun-asahi-group-table)))
	 (national (cons (format (car default) "national") (cdr default)))
	 (paperback (list
		     (concat
		      "<a" s1 "href=\"/"
		      ;; 1. url
		      "\\(paperback/"
		      ;; 2. serial number
		      "\\([a-z]*"
		      ;; 3. year
		      "\\(20[0-9][0-9]\\)"
		      ;; 4. month
		      "\\([01][0-9]\\)"
		      ;; 5. day
		      "\\([0-3][0-9]\\)"
		      "[0-9]+\\)"
		      "\\.html\\)"
		      "\"" s0 ">" s0
		      ;; 6. subject
		      "\\(" no-nl "\\)"
		      s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		     1 2 nil 6 3 4 5))
	 (politics (cons (format (car default) "politics") (cdr default)))
	 (shopping (list
		    (concat
		     "<a" s1 "href=\"/"
		     ;; 1. url
		     "\\(shopping/\\(?:[^\"./>]+/\\)+"
		     ;; 2. serial number
		     "\\([a-z]*"
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "[0-9]*\\)"
		     "\\.html\\)"
		     "\">" s0
		     ;; 6. subject
		     "\\([^<]+\\)"
		     "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		    1 nil 2 6 3 4 5))
	 (shopping2 (list
		     (concat
		      "<a" s1 "href=\"/"
		      ;; 1. url
		      "\\(shopping/yakimono/\\(?:ono\\|yellin\\)/"
		      ;; 2. serial number
		      "\\([a-z]*"
		      ;; 3. year
		      "\\(20[0-9][0-9]\\)"
		      ;; 4. month
		      "\\([01][0-9]\\)"
		      ;; 5. day
		      "\\([0-3][0-9]\\)"
		      "[0-9]*\\)"
		      "\\.html\\)"
		      "\">\\(?:" s0
		      "<div" s1 "class=\"keyword\">[^<]+</div>\\)?" s0
		      ;; 6. subject
		      "\\(" no-nl "\\)"
		      "\\(?:" s0 "&#[0-9]+;\\|&#[0-9]+;" s0 "\\)*"
		      "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		     1 nil 2 6 3 4 5))
	 (sports (shimbun-asahi-make-regexp "sports.spo"))
	 (travel (list
		  (concat "<a" s1 "href=\"/"
			  ;; 1. url
			  "\\(%s/"
			  ;; 2. serial number
			  "\\([a-z]*"
			  ;; 3. year
			  "\\(20[0-9][0-9]\\)"
			  ;; 4. month
			  "\\([01][0-9]\\)"
			  ;; 5. day
			  "\\([0-3][0-9]\\)"
			  "[0-9]+\\)"
			  "\\.html\\)"
			  "\">" s0
			  ;; 6. subject
			  "\\([^<]+\\)"
			  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		  1 nil 2 6 3 4 5)))
    `(("book.column"
       ("$BCx<T$K2q$$$?$$(B" "http://book.asahi.com/author/"
	,(format (car book1) "author") ,@(cdr book1))
       ("$BGd$l$F$kK\(B" "http://book.asahi.com/bestseller/"
	,(format (car book1) "bestseller") ,@(cdr book1))
       ("$B0&$G$?$$J88K(B" "http://book.asahi.com/bunko/"
	,(format (car book1) "bunko") ,@(cdr book1))
       ("$B%S%8%M%9=q(B" "http://book.asahi.com/business/"
	,(format (car book1) "business") ,@(cdr book1))
       ("$B%3%_%C%/%,%$%I(B" "http://book.asahi.com/comic/"
	,(format (car book1) "comic") ,@(cdr book1))
       ("$BOCBj$NK\C*(B" "http://book.asahi.com/hondana/"
	,(format (car book1) "hondana") ,@(cdr book1))
       ("$BJk$i$7$N$*LrN)$A(B" "http://book.asahi.com/life/"
	,(format (car book1) "life") ,@(cdr book1))
       ("$B$?$$$;$D$JK\(B" "http://book.asahi.com/mybook/"
	,(format (car book1) "mybook") ,@(cdr book1))
       ("$B%K%e!<%9$J?74)(B" "http://book.asahi.com/newstar/"
	,(format (car book1) "newstar") ,@(cdr book1))
       ("$B?7=q$N7j(B" "http://book.asahi.com/shinsho/"
	,(format (car book1) "shinsho") ,@(cdr book1))
       ("$B%K%e!<%9$JK\(B" "http://book.asahi.com/topics/"
	,(format (car book1) "topics") ,@(cdr book1))
       ("$B%G%8%?%kFI=q(B" "http://book.asahi.com/trendwatch/"
	,(format (car book1) "trendwatch") ,@(cdr book1)))
      ("book.news"
       ("$BD+F|?7J9<R$N?74)(B" "http://book.asahi.com/asahi/"
	,(format (car book1) "asahi") ,@(cdr book1))
       ("$B$R$H!&N.9T!&OCBj(B" "http://book.asahi.com/clip/"
	,(format (car book1) "clip") ,@(cdr book1))
       ("$B%*%s%i%$%s%V%C%/%U%'%"(B" "http://book.asahi.com/fair/"
	,(format (car book1) "fair") ,@(cdr book1)))
      ("book.paperback"
       ("$BJ88K(B" "http://book.asahi.com/paperback/bunko.html" ,@paperback)
       ("$B?7=q(B" "http://book.asahi.com/paperback/shinsho.html" ,@paperback))
      ("book.review"
       ("$B%S%8%M%9(B" "http://book.asahi.com/review/business.html" ,@book2)
       ("$B%G%8%?%k(B" "http://book.asahi.com/review/digital.html" ,@book2)
       ("$B650i(B ($B;yF8=q(B)" "http://book.asahi.com/review/edu.html" ,@book2)
       ("$B9q:](B" "http://book.asahi.com/review/international.html" ,@book2)
       ("$BJk$i$7(B" "http://book.asahi.com/review/life.html" ,@book2))
      ("book.special"
       ("BOOK TIMES" "http://book.asahi.com/booktimes/"
	,(format (car book1) "booktimes") ,@(cdr book1))
       ("$BGd$l6Z%i%s%-%s%0(B" "http://book.asahi.com/ranking/"
	,(format (car book1) "ranking") ,@(cdr book1)))
      ("business"
       ("$B#A#E#R#AH/%^%M!<(B" "http://www.asahi.com/business/aera/"
	,(format (car business) "aera") ,@(cdr business))
       ("$BEj;q?.Bw(B" "http://www.asahi.com/business/fund/"
	,(format (car business) "fund") ,@(cdr business))
       ("$B>&IJ%U%!%$%k(B" "http://www.asahi.com/business/products/"
	,(format (car business) "products") ,@(cdr business))
       ("$B%m%$%?!<%K%e!<%9(B" "http://www.asahi.com/business/list_reuters.html"
	,(format (car business) "reuters") ,@(cdr business))
       ("$B:#F|$N;kE@(B" "http://www.asahi.com/business/today_eye/"
	,(format (car business) "today_eye") ,@(cdr business))
       ("$B:#F|$N;T67(B" "http://www.asahi.com/business/today_shikyo/"
	,(format (car business) "today_shikyo") ,@(cdr business))
       ("$B7P:Q$rFI$`(B" "http://www.asahi.com/business/topics/"
	,(format (car business) "topics") ,@(cdr business))
       ("$BElMN7P:Q%K%e!<%9(B" "http://www.asahi.com/business/list_toyo.html"
	,(format (car business) "toyo") ,@(cdr business)))
      ("car"
       ("$B?7<VH/I=2q(B" "http://www.asahi.com/car/cg/"
	,@(shimbun-asahi-make-regexp "car.cg"))
       ("$B%$%?%j%"H/%"%b!<%l!*%b%H!<%l!*(B"
	"http://www.asahi.com/car/italycolumn/"
	,@(shimbun-asahi-make-regexp "car.italycolumn"))
       ("$B%b!<%?!<%9%]!<%D(B" "http://www.asahi.com/car/motorsports/"
	,@(shimbun-asahi-make-regexp "car.motorsports"))
       ("$B?7<V>pJs(B" "http://www.asahi.com/car/newcar/"
	,@(shimbun-asahi-make-regexp "car.newcar")))
      ("culture"
       ("$BJ82=(B" "http://www.asahi.com/culture/list_culture.html" ,@culture)
       ("$B7]G=(B" "http://www.asahi.com/culture/list_entertainment.html"
	,@culture)
       ("$BF#Bt<~J?$N@$3&(B" "http://www.asahi.com/culture/fujisawa/"
	,@(shimbun-asahi-make-regexp "culture.fujisawa"))
       ("$B?M4V9qJu(B" "http://www.asahi.com/culture/kokuhou/"
	,@(shimbun-asahi-make-regexp "culture.kokuhou"))
       ("$B$$$D$+$OL>?M2q(B" "http://www.asahi.com/culture/column/rakugo/guide/"
	,@(shimbun-asahi-make-regexp "culture.column.rakugo.guide"))
       ("$BMn8l$C$F(B" "http://www.asahi.com/culture/column/rakugo/kyosu/"
	,@(shimbun-asahi-make-regexp "culture.column.rakugo.kyosu"))
       ("$B%i%/%4%m%/(B" "http://www.asahi.com/culture/column/rakugo/rakugoroku/"
	,@(shimbun-asahi-make-regexp "culture.column.rakugo.rakugoroku"))
       ("$BO":\5-;v(B" "http://www.asahi.com/culture/serial_backnumber.html"
	,@culture)
       ("$B$f$k$f$k%U%'%_%K%s(B" "http://www.asahi.com/culture/column/yurufemi/"
	,@(shimbun-asahi-make-regexp "culture.column.yurufemi")))
      ("digital"
       ("$B5!4o(B" "http://www.asahi.com/digital/av/"
	,@(shimbun-asahi-make-regexp "digital.av"))
       ("e-$B%S%8%M%9>pJs(B ($BDs6!(B: BCN)" "http://www.asahi.com/digital/bcnnews/"
	,@(shimbun-asahi-make-regexp "digital.bcnnews"))
       ("$B%3%i%`(B" "http://www.asahi.com/digital/column01/"
	,@(shimbun-asahi-make-regexp "digital.column01"))
       ("$B%M%C%H!&%&%$%k%9(B" "http://www.asahi.com/digital/internet/"
	,@(shimbun-asahi-make-regexp "digital.internet"))
       ("$B7HBSEEOC(B" "http://www.asahi.com/digital/mobile/"
	,@(shimbun-asahi-make-regexp "digital.mobile"))
       ("$BF|4)9)6H?7J9%K%e!<%9(B" "http://www.asahi.com/digital/nikkanko/"
	,@(shimbun-asahi-make-regexp "digital.nikkanko"))
       ("$B#P#C!&%2!<%`(B" "http://www.asahi.com/digital/pc/"
	,@(shimbun-asahi-make-regexp "digital.pc")))
      ("edu"
       ("$BF~;n(B" "http://www.asahi.com/edu/news/examination.html" ,@edu)
       ("$B$f$-09$N;R0i$F1~1g%(%C%;!<(B" "http://www.asahi.com/edu/column/ikuji/"
	,@(shimbun-asahi-make-regexp "edu.column.ikuji"))
       ("$B650iLdBj(B" "http://www.asahi.com/edu/news/issue.html" ,@edu)
       ("$B$3$N5-;v$r<j$,$+$j$K(B" "http://www.asahi.com/edu/nie/kiji/"
	,@(shimbun-asahi-make-regexp "edu.nie.kiji.kiji"))
       ("$B;R0i$F(B" "http://www.asahi.com/edu/news/kosodate.html" ,@edu)
       ("$B650i@)EY!&OCBj(B" "http://www.asahi.com/edu/news/system.html" ,@edu)
       ("$B$N$N$A$c$s$N#D#O2J3X(B" "http://www.asahi.com/edu/nie/tamate/"
	,@(shimbun-asahi-make-regexp "edu.nie.tamate.kiji"))
       ("$BBg3X(B" "http://www.asahi.com/edu/news/university.html" ,@edu))
      ("food"
       ("$B5(@a$N$*$$$7$$%3%i%`(B" "http://www.asahi.com/food/cooking/"
	,@(shimbun-asahi-make-regexp "food.cooking"))
       ("$B4_D+;R!VJk$i$7$r3Z$7$`$*<h$j4s$;!W(B"
	"http://www.asahi.com/shopping/food/kishi/"
	,(concat "<a" s1 "href=\"/"
		 ;; 1. url
		 "\\(shopping/food/kishi/"
		 ;; 2. serial number
		 "\\([a-z]*"
		 ;; 3. year
		 "\\(20[0-9][0-9]\\)"
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 "[0-9]+\\)"
		 "\\.html\\)"
		 "\">" s0
		 ;; 6. subject
		 "\\(" no-nl "\\)"
		 "\\(?:" s0 "<[^>]+>\\)*[^<]*([01]?[0-9]/[0-3]?[0-9])")
	1 nil 2 6 3 4 5)
       ("$BNAM}%a%b(B" "http://www.asahi.com/food/memo/"
	,@(shimbun-asahi-make-regexp "food.memo"))
       ("$B!V?@$N<6!W:n<T$N%N%`%j%(F|5-(B"
	"http://www.asahi.com/food/column/nommelier/"
	,@(shimbun-asahi-make-regexp "food.column.nommelier"))
       ("$B$*$$$7$5H/8+(B" "http://www.asahi.com/food/column/oishisa/"
	,@(shimbun-asahi-make-regexp "food.column.oishisa"))
       ("$BO@$h$j!"$*$d$D(B" "http://www.asahi.com/food/column/oyatsu/"
	,@(shimbun-asahi-make-regexp "food.column.oyatsu"))
       ("$B%9%$!<%D$N?4F@(B" "http://www.asahi.com/food/column/sweets/"
	,@(shimbun-asahi-make-regexp "food.column.sweets"))
       ("$B%o%$%s$N:P;~5-(B" "http://www.asahi.com/food/column/wine_saijiki/"
	,@(shimbun-asahi-make-regexp "food.column.wine_saijiki")))
      ("health"
       ("$B7r9/!&@83h(B" "http://www.asahi.com/health/news/" ,@health)
       ("$BJ!;c!&9bNp(B" "http://www.asahi.com/health/news/aged.html" ,@health)
       ("$BG'CN>IFC=8(B" "http://www.asahi.com/health/news/alz.html" ,@health)
       ("$B0eNE!&IB5$(B" "http://www.asahi.com/health/news/medical.html" ,@health))
      ("housing"
       ("$B8M7z$F(B" "http://www.asahi.com/ad/clients/kodatenavi/news/"
	,@(shimbun-asahi-make-regexp "ad.clients.kodatenavi.news"))
       ("$B%^%s%7%g%s(B" "http://www.asahi.com/ad/clients/mansionnavi/news/"
	,@(shimbun-asahi-make-regexp "ad.clients.mansionnavi.news"))
       ("$BE7Ln>4$N$$$$2H$$$$2HB2(B" "http://www.asahi.com/housing/amano/"
	,@(shimbun-asahi-make-regexp "housing.amano"))
       ("$B=;$^$$$N$*LrN)$A%3%i%`(B" "http://www.asahi.com/housing/column/"
	,@(shimbun-asahi-make-regexp "housing.column"))
       ("$B>.$5$J2H$N@83hF|5-(B" "http://www.asahi.com/housing/diary/"
	,@(shimbun-asahi-make-regexp "housing.diary"))
       ("$B=;Bp?7Js<R%K%e!<%9(B" "http://www.asahi.com/housing/jutaku-s/"
	,@(shimbun-asahi-make-regexp "housing.jutaku-s"))
       ("$B$3$3$,CN$j$?$$(B" "http://www.asahi.com/housing/soudan/index_s.html"
	,@(shimbun-asahi-make-regexp "housing.soudan"))
       ("$B@$3&$N%&%A(B" "http://www.asahi.com/housing/world/"
	,@(shimbun-asahi-make-regexp "housing.world")))
      ("igo"
       ("$B%H%T%C%/%9(B" "http://www.asahi.com/igo/topics/"
	,@(shimbun-asahi-make-regexp "igo.topics")))
      ("international.asia"
       ("$B%"%8%"$N393Q(B" "http://www.asahi.com/international/asiamachi/"
	,@(shimbun-asahi-make-regexp "international.asiamachi"))
       ("$B?ML1F|Js(B" "http://www.asahi.com/international/jinmin/"
	,@(shimbun-asahi-make-regexp "international.jinmin"))
       ("$B%3%j%"$&$a!<$d!*!*(B" "http://www.asahi.com/international/korea/"
	,@(shimbun-asahi-make-regexp "international.korea"))
       ("$B%9%Q%$%7!<!*%=%&%k(B" "http://www.asahi.com/international/seoul/"
	,@(shimbun-asahi-make-regexp "international.seoul"))
       ("$B=54)%"%8%"(B" "http://www.asahi.com/international/weekly-asia/"
	,@(shimbun-asahi-make-regexp "international.weekly-asia")))
      ("international.column"
       ("$BA%66MN0l$N@$3&%V%j!<%U%#%s%0(B"
	"http://opendoors.asahi.com/syukan/briefing/index.shtml"
	,(concat
	  "<a href=\""
	  ;; 1. url
	  "\\("
	  ;; 2. serial number
	  "\\([0-9]+\\)"
	  "\\.shtml\\)"
	  "\">No\\.[0-9]+$B!!(B\\[ $B=54)D+F|(B"
	  ;; 3. year
	  "\\(20[0-9][0-9]\\)"
	  "$BG/(B"
	  ;; 4. month
	  "\\([01]?[0-9]\\)"
	  "$B7n(B"
	  ;; 5. day
	  "\\([0-3]?[0-9]\\)"
	  "[^]]+\\] <br>"
	  ;; 6. subject
	  "\\([^<]+\\)")
	1 2 nil 6 3 4 5)
       ("$B;Q7n$"$5$H$N!VFH$j8@!W(B" "http://www.asahi.com/international/shizuki/"
	,@(shimbun-asahi-make-regexp "international.shizuki")))
      ("international.special"
       ("$B9q:];Y1g$N8=>l$+$i(B" "http://www.asahi.com/international/shien/"
	,(concat
	  "$B!Z!w(B[^$B![(B]+$B![(B[\t\n -]*<a" s1 "href=\"/"
	  ;; 1. url
	  "\\(international/shien/"
	  ;; 2. serial number
	  "\\([a-z]*"
	  ;; 3. year
	  "\\(20[0-9][0-9]\\)"
	  ;; 4. month
	  "\\([01][0-9]\\)"
	  ;; 5. day
	  "\\([0-3][0-9]\\)"
	  "[0-9]+\\)"
	  "\\.html\\)"
	  "\">\\(?:" s0 "<[^>]+>\\)*" s0
	  ;; 6. subject
	  "\\([^\n<>]+\\)")
	1 nil 2 6 3 4 5)
       ("$BD;%$%s%U%k%(%s%6(B" "http://www.asahi.com/special/051102/"
	,@(shimbun-asahi-make-regexp "special.051102"))
       ("$BF|Cf4X78(B" "http://www.asahi.com/special/050410/"
	,@(shimbun-asahi-make-regexp "special.050410"))
       ("$BCO5e4D6-(B" "http://www.asahi.com/special/070110/"
	,@(shimbun-asahi-make-regexp "special.070110"))
       ("$BKLD+A/YGCW;v7o(B" "http://www.asahi.com/special/abductees/"
	,@(shimbun-asahi-make-regexp "special.abductees"))
       ("$B#B#S#ELdBj(B" "http://www.asahi.com/special/bse/"
	,@(shimbun-asahi-make-regexp "special.bse"))
       ("$B%$%i%/>p@*(B" "http://www2.asahi.com/special/iraq/"
	,@(shimbun-asahi-make-regexp "special.iraq"))
       ("$BCfElOBJ?(B" "http://www.asahi.com/special/MiddleEast/"
	,@(shimbun-asahi-make-regexp "special.MiddleEast"))
       ("$BKLD+A/3KLdBj(B" "http://www.asahi.com/special/nuclear/"
	,@(shimbun-asahi-make-regexp "special.nuclear"))
       ("$BCf9qFC=8(B" "http://www.asahi.com/world/china/"
	,@(shimbun-asahi-make-regexp "world.china.news")))
      ("international.world"
       ("$B%"%U%j%+(B" "http://www.asahi.com/international/africa.html"
	,@international)
       ("$B9qO"!&$=$NB>(B" "http://www.asahi.com/international/etc.html"
	,@international)
       ("$B%h!<%m%C%Q(B" "http://www.asahi.com/international/europe.html"
	,@international)
       ("$BCfEl(B" "http://www.asahi.com/international/middleeast.html"
	,@international)
       ("$BKLJF(B" "http://www.asahi.com/international/namerica.html"
	,@international)
       ("$B%*%;%"%K%"(B" "http://www.asahi.com/international/oceania.html"
	,@international)
       ("$BCfFnJF(B" "http://www.asahi.com/international/samerica.html"
	,@international))
      ("job"
       ("$B=54)D+F|!&#A#E#R#A$+$i(B" "http://www.asahi.com/job/special/"
	,@(let ((def (shimbun-asahi-make-regexp "job.special")))
	    (cons (concat (car def)
			  "\\(?:" s0 "<[^>]+>\\)*" s0 "$B!J(B" s0
			  ;; 7. extra
			  "\\(" no-nl "\\)"
			  "$B!'(B")
		  (append (cdr def) '(nil 7))))))
      ("kansai"
       ("$B4X@>7]G=%K%e!<%9(B" "http://www.asahi.com/kansai/entertainment/news/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.news"))
       ("$B%$%Y%s%H(B" "http://www.asahi.com/kansai/event/"
	,@(shimbun-asahi-make-regexp "kansai.event")))
      ("kansai.entertainment"
       ("$BJFD+8}$^$+$;(B" "http://www.asahi.com/kansai/entertainment/beichou/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.beichou"))
       ("$B>!<j$K4X@>@$3&0d;:(B"
	"http://www.asahi.com/kansai/entertainment/kansaiisan/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.kansaiisan"))
       ("$B65$($F!*4X@>$C;R(B"
	"http://www.asahi.com/kansai/entertainment/kansaikko/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.kansaikko"))
       ("$B;0;^$N>P%&%$%s%I%&(B"
	"http://www.asahi.com/kansai/entertainment/sanshi/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.sanshi"))
       ("$B@i5RK|:P!*(B" "http://www.asahi.com/kansai/entertainment/senkyaku/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.senkyaku")))
      ("kansai.kokoro"
       ("$B!{!{$N$R$_$D(B" "http://www.asahi.com/kansai/kokoro/himitsu/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.himitsu"))
       ("$B5'$j$NH~(B" "http://www.asahi.com/kansai/kokoro/inori/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.inori"))
       ("$B%T%s%[!<%k$NL\(B" "http://www.asahi.com/kansai/kokoro/pinhole/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.pinhole"))
       ("$B8l$j$"$&(B" "http://www.asahi.com/kansai/kokoro/taidan/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.taidan"))
       ("$B$H$_$3$&$_(B" "http://www.asahi.com/kansai/kokoro/tomikoumi/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.tomikoumi")))
      ("kansai.sumai"
       ("$B$W$i$C$H1h@~5*9T(B" "http://www.asahi.com/kansai/sumai/ensen/"
	,@(shimbun-asahi-make-regexp "kansai.sumai.ensen"))
       ("$B39$rNx$&(B" "http://www.asahi.com/kansai/sumai/machi/"
	,@(shimbun-asahi-make-regexp "kansai.sumai.machi"))
       ("$B4X@>$N=;$^$$(B" "http://www.asahi.com/kansai/sumai/news/"
	,@(shimbun-asahi-make-regexp "kansai.sumai.news")))
      ("kansai.taberu"
       ("$B%G%QCO2<#N#E#W#S(B" "http://www.asahi.com/kansai/taberu/depa/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.depa"))
       ("$B5(8l$r$$$?$@$/(B" "http://www.asahi.com/kansai/taberu/kigo/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.kigo"))
       ("$B$^$s$W$/2q$N%i%s%A%?%$%`(B"
	"http://www.asahi.com/kansai/taberu/manpuku/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.manpuku"))
       ("$B%Q!<%s$H%9%$!<%D(B" "http://www.asahi.com/kansai/taberu/pan_sweets/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.pan_sweets"))
       ("$B$d$5$7$$:h(B" "http://www.asahi.com/kansai/taberu/sakana/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.sakana")))
      ("life.column"
       ("$B?4CO$h$$@83h$NCN7C(B" "http://www.asahi.com/life/column/chie/"
	,@(shimbun-asahi-make-regexp "life.column.chie"))
       ("$B;d$N%_%+%?(B" "http://www.asahi.com/life/column/mikata/"
	,@(shimbun-asahi-make-regexp "life.column.mikata\\(?:/sasaki\\)?"))
       ("$B2.86Gn;R$N!H$,$s$P$l!*2H7W!I(B"
	"http://www.asahi.com/life/column/ogiwara/"
	,@(shimbun-asahi-make-regexp "life.column.ogiwara")))
      ("national"
       ("$B:R32!&8rDL>pJs(B" "http://www.asahi.com/national/calamity.html"
	,@national)
       ("$B$=$NB>!&OCBj(B" "http://www.asahi.com/national/etc.html" ,@national)
       ("$B;v7o!&;v8N(B" "http://www.asahi.com/national/incident.html" ,@national)
       ("$B:[H=(B" "http://www.asahi.com/national/trial.html" ,@national)
       ("$B$*$/$d$_(B" "http://www.asahi.com/obituaries/"
	,(format (car default) "obituaries") ,@(cdr default)))
      ("politics"
       ("$B9q@/(B" "http://www.asahi.com/politics/government.html" ,@politics)
       ("$BCOJ}@/<#(B" "http://www.asahi.com/politics/local.html" ,@politics))
      ("shopping.column"
       ("$B3ZE7$3$@$o$jE9D9$KJ9$/(B" "http://www.asahi.com/shopping/column/master/"
	,@shopping)
       ("$BJk$i$7$r3Z$7$`(B" "http://www.asahi.com/shopping/column/tatsujin/"
	,@shopping))
      ("shopping.yakimono"
       ("$B>.Ln8x5W!V$d$-$b$N%,%$%I!W(B"
	"http://www.asahi.com/shopping/yakimono/ono/" ,@shopping2)
       ("$B%m%P!<%H!&%$%(%j%s!V$d$-$b$N;6JbF;!W(B"
	"http://www.asahi.com/shopping/yakimono/yellin/" ,@shopping2))
      ("shougi"
       ("$BK\(B" "http://www.asahi.com/shougi/books/"
	,@(shimbun-asahi-make-regexp "shougi.books"))
       ("$B%K%e!<%9(B" "http://www.asahi.com/shougi/news/"
	,@(shimbun-asahi-make-regexp "shougi.news"))
       ("$B%H%T%C%/%9(B" "http://www.asahi.com/shougi/topics/"
	,@(shimbun-asahi-make-regexp "shougi.topics")))
      ("sports.baseball"
       ("$B%"%^%A%e%"Ln5e(B" "http://www.asahi.com/sports/bb/ama.html" ,@baseball)
       ("$BBg%j!<%0(B" "http://www.asahi.com/sports/bb/mlb.html" ,@baseball)
       ("$B%W%mLn5e(B" "http://www.asahi.com/sports/bb/pro.html" ,@baseball))
      ("sports.battle"
       ("$B3JF.5;(B" "http://www.asahi.com/sports/spo/battle.html"
	,@(shimbun-asahi-make-regexp "sports.\\(?:column\\|spo\\)"))
       ("$BAjKP(B" "http://www.asahi.com/sports/spo/sumo.html" ,@sports))
      ("sports.etc"
       ("$B%3%i%`(B" "http://www.asahi.com/sports/column/"
	,@(shimbun-asahi-make-regexp "sports.column"))
       ("$B9q300lHL%9%]!<%D(B" "http://www.asahi.com/sports/spo/kaigai.html"
	,@sports)
       ("$B9qFb0lHL%9%]!<%D(B" "http://www.asahi.com/sports/spo/kokunai.html"
	,@sports)
       ("$B%l!<%7%s%0(B" "http://www.asahi.com/sports/spo/motor.html" ,@sports))
      ("sports.football"
       ("$BF|K\BeI=(B" "http://www.asahi.com/sports/fb/japan/list.html"
	,@(shimbun-asahi-make-regexp "sports.fb.japan"))
       ("$B#J%j!<%0!&9qFb(B" "http://www.asahi.com/sports/fb/national.html"
	,@football)
       ("$B3$30(B" "http://www.asahi.com/sports/fb/world.html" ,@football))
      ("sports.golf"
       ("$BCK;R%4%k%U(B" "http://www.asahi.com/sports/spo/golf_man.html" ,@sports)
       ("$B=w;R%4%k%U(B" "http://www.asahi.com/sports/spo/golf_woman.html"
	,@sports))
      ("sports.rugby"
       ("$BF|K\BeI=(B" "http://www.asahi.com/sports/spo/rugby_japan.html" ,@sports)
       ("$B9qFb!&$=$NB>(B" "http://www.asahi.com/sports/spo/rugby_national.html"
	,@sports))
      ("sports.usa"
       ("$B#N#B#A(B" "http://www.asahi.com/sports/spo/nba.html" ,@sports)
       ("$B#N#F#L(B" "http://www.asahi.com/sports/spo/nfl.html" ,@sports)
       ("$B#N#H#L(B" "http://www.asahi.com/sports/spo/nhl.html" ,@sports))
      ("sports.winter"
       ("$B%9%1!<%H!&I9>e6%5;(B" "http://www.asahi.com/sports/spo/skate.html"
	,@sports)
       ("$B%9%-!<!&@c>e6%5;(B" "http://www.asahi.com/sports/spo/ski.html"
	,@sports))
      ("travel"
       ("$BN9$9$k?M$N%"%Z%j%F%#%U(B" "http://www.asahi.com/travel/aperitif/"
	,(format (car travel) "travel/aperitif") ,@(cdr travel))
       ("$B$]$l$]$l%5%U%!%j(B" "http://www.asahi.com/travel/porepore/"
	,@(shimbun-asahi-make-regexp "travel.porepore"))
       ("$BEgN9$?$S(B" "http://www.asahi.com/travel/shima/"
	,(format (car travel) "travel/shima") ,@(cdr travel))
       ("$B=54)%7%k%/%m!<%I5*9T(B" "http://www.asahi.com/travel/silkroad/"
	,@(shimbun-asahi-make-regexp "travel.silkroad"))
       ("$B0&$NN9?M(B" "http://www.asahi.com/travel/traveler/"
	,(format (car travel) "travel/traveler") ,@(cdr travel)))))
  "Alist of parent groups and lists of tables for subgroups.
Each table is the same as the `cdr' of the element of
`shimbun-asahi-group-table'.")

(defvar shimbun-asahi-content-start
  "<div[\t\n ]+class=\"\
\\(?:ThmbSet300Tb\\|ThmbSet256\\|Kansai-ThmbSet100\\|ThmbCol\\)\">\
\\|<!--[\t\n ]*End of Headline[\t\n ]*-->\
\\(?:[\t\n ]*<div[\t\n ]+[^<]+</div>[\t\n ]*\
\\|[\t\n ]*<p[\t\n ]+[^<]+</p>[\t\n ]*\\)?\
\\|<!--[\t\n ]*Start of \\(Kiji\\|photo\\)[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-content-end
  "<dl[\t\n ]+class=\"PrInfo\">\
\\|<!--[\t\n ]*google_ad_section_end\
\\|<!-[^>]+[^>$B!z(B]$B$3$3$^$G(B[\t\n ]*-+>\
\\|\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--[\t\n ]*Start of hatenab[\t\n ]*-->\
\\|<!--[\t\n ]*End of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*End of related link[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-text-content-start
  "<div[\t\n ]+class=\"\\(?:ThmbSet256\\|Kansai-ThmbSet100\\|ThmbCol\\)\">\
\\|<!--[\t\n ]*End of Headline[\t\n ]*-->\
\\(?:[\t\n ]*<div[\t\n ]+[^<]+</div>[\t\n ]*\
\\|[\t\n ]*<p[\t\n ]+[^<]+</p>[\t\n ]*\\)?\
\\|<!--[\t\n ]*Start of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-text-content-end
  "<dl[\t\n ]+class=\"PrInfo\">\
\\|<!--[\t\n ]*google_ad_section_end\
\\|<!-[^>]+$B$3$3$^$G(B[\t\n ]*-+>\
\\|\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--[\t\n ]*Start of hatenab[\t\n ]*-->\
\\|<!--[\t\n ]*\\(?:google_ad_section\\|[AD$B!z!y(B]+\\)\
\\|<!--[\t\n ]*End of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAAEIAAAAQBAMAAABQPLQnAAAAElBMVEX8rKjd3Nj+7utdXFr
 ////oOTn0UMNfAAAA90lEQVQoz21SQWoDMQwUprlXWA8IZvsAd/yA2KsHLKX+/1c6cklI2EgGeTW
 jwYxW/F2Uq0zGIcWHPAO5ut+SF7nMeyQ/Mb6OqwhnGSOE0gtjb9Q4JG33xqCYeM/mvVqUXOtY+Lp
 37XqbIp4BB9p/wef8IAF1BxQRRoaiIzcWQ4Vdfp2f0aiItKXRSWUxjhk1OjqR3NDYMomBkEVIdYy
 56Q6SbOEKlz0kQy5jZ95mYVvrQpWN0ADB9axMje/5QzAeXvsSbbQvqavxKA3QIpt655XHmOphcHo
 YJJJOO5Kw7QiXSrh9JnAv47GmJ7tfGD7Wqrnp7e2f8AfqrGxn9j9f+QAAAABJRU5ErkJggg==")))
;;  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
;;bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
;;7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(defvar shimbun-asahi-expiration-days 6)

(luna-define-method initialize-instance :after ((shimbun shimbun-asahi)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "$BD+F|?7J9(B")
  (shimbun-set-from-address-internal shimbun "nobody@example.com")
  ;; To share class variables between `shimbun-asahi' and its
  ;; successor `shimbun-asahi-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-asahi-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun shimbun-asahi-expiration-days)
  (shimbun-set-content-start-internal shimbun shimbun-asahi-content-start)
  (shimbun-set-content-end-internal shimbun shimbun-asahi-content-end)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-asahi))
  (mapcar 'car shimbun-asahi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-asahi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (or (nth 2 (assoc group shimbun-asahi-group-table))
		    (concat (shimbun-subst-char-in-string ?. ?/ group) "/"))))
    (cond ((not index)
	   "about:blank")
	  ((string-match "\\`http:" index)
	   index)
	  ((string-match "\\`book\\." group)
	   (shimbun-expand-url (substring index 5) "http://book.asahi.com/"))
	  (t
	   (shimbun-expand-url (format index group) shimbun-asahi-url)))))

(defun shimbun-asahi-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	regexp jname numbers book-p cyear cmonth rss-p paper-p en-category
	hour-min month year day serial num extra rgroup id headers
	travel-p subgroups iraq-p)
    (setq regexp (assoc group shimbun-asahi-group-table)
	  jname (nth 1 regexp)
	  numbers (nthcdr 4 regexp)
	  book-p (string-match "\\`book\\." group))
    (when (setq regexp (nth 3 regexp))
      (setq regexp (format regexp
			   (regexp-quote (shimbun-subst-char-in-string
					  ?. ?/ (if book-p
						    (substring group 5)
						  group))))))
    (setq cyear (shimbun-decode-time nil 32400)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear)
	  rss-p (member group '("book" "rss"))
	  paper-p (member group '("editorial" "tenjin"))
	  travel-p (string-equal group "travel")
	  subgroups (cdr (assoc group shimbun-asahi-subgroups-alist)))
    (shimbun-strip-cr)
    (goto-char (point-min))
    (catch 'stop
      ;; The loop for fetching all the articles in the subgroups.
      (while t
	(when regexp
	  (while (re-search-forward regexp nil t)
	    (cond ((string-equal group "english")
		   (setq en-category
			 (save-excursion
			   (save-match-data
			     (if (re-search-backward "\
<h[0-9]\\(?:[\n\t ]+[^>]+\\)?>[\t\n ]*\\([^&]+\\)[\t\n ]*&#[0-9]+"
						     nil t)
				 (downcase (match-string 1)))))))
		  (t
		   (setq hour-min
			 (save-excursion
			   (save-match-data
			     (if (re-search-forward "\
<span[\t\n ]+[^>]+>[\t\n ]*(\\(?:[01]?[0-9]/[0-3]?[0-9][\t\n ]+\\)?
\\([012]?[0-9]:[0-5][0-9]\\))[\t\n ]*</span>"
						    nil t)
				 (match-string 1)))))))
	    (setq month (string-to-number (match-string (nth 5 numbers)))
		  year (if (setq num (nth 4 numbers))
			   (string-to-number (match-string num))
			 (cond ((>= (- month cmonth) 2)
				(1- cyear))
			       ((and (= 1 month) (= 12 cmonth))
				(1+ cyear))
			       (t
				cyear)))
		  day (string-to-number (match-string (nth 6 numbers)))
		  serial (cond (rss-p
				(if (match-beginning (nth 1 numbers))
				    (format "%d%s.%s"
					    year
					    (match-string (nth 1 numbers))
					    (match-string (nth 2 numbers)))
				  (match-string (nth 2 numbers))))
			       (paper-p
				(format "%d%02d%02d" year month day))
			       ((and (setq num (nth 1 numbers))
				     (match-beginning num))
				(format "%d%02d%02d.%s"
					year month day (match-string num)))
			       (t
				(shimbun-subst-char-in-string
				 ?/ ?.
				 (downcase (match-string (nth 2 numbers))))))
		  extra (or (and (setq num (nth 8 numbers))
				 (match-beginning num)
				 (match-string num))
			    (and (setq num (nth 9 numbers))
				 (match-beginning num)
				 (match-string num)))
		  rgroup (mapconcat 'identity
				    (nreverse (save-match-data
						(split-string group "\\.")))
				    ".")
		  id (if (and extra
			      (not (save-match-data
				     (string-match "$B=54)D+F|!&#A#E#R#A$+$i(B"
						   from))))
			 (concat "<" serial "%" extra "." rgroup "."
				 shimbun-asahi-top-level-domain ">")
		       (concat "<" serial "%" rgroup "."
			       shimbun-asahi-top-level-domain ">")))
	    (unless (shimbun-search-id shimbun id)
	      (push (shimbun-create-header
		     ;; number
		     0
		     ;; subject
		     (cond (rss-p
			    (match-string (nth 3 numbers)))
			   (en-category
			    (concat "[" en-category "] "
				    (match-string (nth 3 numbers))))
			   ((and (setq num (nth 8 numbers))
				 (match-beginning num))
			    (concat "[" (match-string num) "] "
				    (match-string (nth 3 numbers))))
			   ((and (setq num (nth 9 numbers))
				 (match-beginning num))
			    (concat "[" (match-string num) "] "
				    (match-string (nth 3 numbers))))
			   (paper-p
			    (concat jname (format " (%d/%d)" month day)))
			   (travel-p
			    (save-match-data
			      (shimbun-replace-in-string
			       (match-string (nth 3 numbers))
			       "\\(?:[\t\n $B!!(B]*&#[0-9]+;\\)*[\t\n $B!!(B]*" "")))
			   (t
			    (match-string (nth 3 numbers))))
		     ;; from
		     (if (and rss-p
			      (setq num (nth 10 numbers))
			      (setq num (match-string num)))
			 (save-match-data
			   (when (and book-p
				      (string-match
				       "\\`$B=qI>!!(B\\[$BI><T(B\\]\\($B$=$NB>(B\\)?" num))
			     (setq num (if (match-beginning 1)
					   "$B=qI>(B"
					 (substring num (match-end 0)))))
			   (shimbun-replace-in-string
			    from "(RSS" (concat "(" num)))
		       from)
		     ;; date
		     (shimbun-make-date-string
		      year month day
		      (cond ((and (setq num (nth 11 numbers))
				  (match-beginning num))
			     (concat (match-string num) ":"
				     (match-string (nth 12 numbers))))
			    ((and (setq num (nth 7 numbers))
				  (match-beginning num))
			     (match-string num))
			    (paper-p
			     "07:00")
			    (t
			     hour-min)))
		     ;; id
		     id
		     ;; references, chars, lines
		     "" 0 0
		     ;; xref
		     (shimbun-expand-url
		      (match-string (nth 0 numbers))
		      (cond (paper-p
			     (concat shimbun-asahi-url "paper/"))
			    (book-p
			     "http://book.asahi.com/")
			    ((string-match "$BA%66MN0l$N@$3&%V%j!<%U%#%s%0(B" from)
			     "http://opendoors.asahi.com/syukan/briefing/")
			    (iraq-p
			     "http://www2.asahi.com/")
			    (t
			     shimbun-asahi-url))))
		    headers)))
	  (when (string-match "$B;d$N%_%+%?(B" from)
	    (setq headers (nreverse headers))))
	(if subgroups
	    (progn
	      (erase-buffer)
	      (setq from (concat (shimbun-server-name shimbun)
				 " (" (caar subgroups) ")")
		    iraq-p (string-equal (caar subgroups) "$B%$%i%/>p@*(B"))
	      (shimbun-retrieve-url (cadar subgroups))
	      (setq regexp (caddar subgroups)
		    numbers (cdddar subgroups)
		    subgroups (cdr subgroups)))
	  (throw 'stop nil))))
    (append (shimbun-sort-headers headers)
	    (shimbun-asahi-get-headers-for-today group jname from))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(defun shimbun-asahi-get-headers-for-today (group jname from)
  "Return a list of the header for today's article.
It works for only the groups `editorial' and `tenjin'."
  (goto-char (point-min))
  (let ((basename (cdr (assoc group '(("editorial" . "editorial")
				      ("tenjin" . "column")))))
	year cmonth month day url)
    (when (and basename
	       (re-search-forward
		(eval-when-compile
		  (concat
		   "/\\(?:editorial\\|column\\)\\.html\"[^\n0-9]+"
		   ;; 1. month
		   "\\([01]?[0-9]\\)" "$B7n(B"
		   ;; 2. day
		   "\\([0-3]?[0-9]\\)" "$BF|IU(B"))
		nil t))
      (setq year (shimbun-decode-time nil 32400)
	    cmonth (nth 4 year)
	    year (nth 5 year)
	    month (string-to-number (match-string 1))
	    day (string-to-number (match-string 2)))
      (cond ((and (= cmonth 1) (= month 12))
	     (decf year))
	    ((and (= cmonth 12) (= month 1))
	     (incf year)))
      (setq url (format "paper/%s%d%02d%02d.html" basename year month day))
      (list
       (shimbun-make-header
	;; number
	0
	;; subject
	(shimbun-mime-encode-string (concat jname
					    (format " (%d/%d)" month day)))
	;; from
	from
	;; date
	(shimbun-make-date-string year month day "07:00")
	;; id
	(format "<%d%02d%02d%%%s.%s>"
		year month day group shimbun-asahi-top-level-domain)
	;; references, chars, lines
	"" 0 0
	;; xref
	(shimbun-expand-url url shimbun-asahi-url))))))

(defun shimbun-asahi-multi-next-url (shimbun header url)
  (goto-char (point-min))
  (when (and (re-search-forward "\
<div[\t\n ]\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"SeqNav forSplit\""
				nil t)
	     (shimbun-end-of-tag "div" t))
    (let ((end (match-beginning 0)))
      (prog1
	  (when (re-search-backward "\
<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*$B<!%Z!<%8(B[\t\n ]*</a>"
				    end 'move)
	    (goto-char end)
	    (shimbun-expand-url (match-string 1) url))
	(if (and (re-search-backward "[^\t\n >]\\([\t\n ]*<\\)" nil t)
		 (re-search-forward "[\t\n ]*<[\t\n ]*[^/]" end t))
	    (delete-region (match-beginning 0) end)
	  (goto-char end))
	(insert "<!-- End of Kiji -->\n")))))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-asahi)
					    header url)
  (shimbun-asahi-multi-next-url shimbun header url))

(defun shimbun-asahi-prepare-article (shimbun header)
  "Prepare an article.
For the groups editorial and tenjin, it tries to fetch the article for
that day if it failed."
  (let ((case-fold-search t)
	(group (shimbun-current-group-internal shimbun))
	(from (shimbun-header-from-internal header)))
    (cond
     ((string-equal group "car")
      (shimbun-remove-tags "\
\[\t\n ]*<![\t\n ]*-+[\t\n ]*[$B!z!y(B]+[\t\n ]*AD[\t\n ]*[$B!z!y(B]+[\t\n ]*-+>"
			   "\
<![\t\n ]*-+[\t\n ]*/[\t\n ]*[$B!z!y(B]+[\t\n ]*AD[\t\n ]*[$B!z!y(B]+[\t\n ]*-+>\
\[\t\n ]*")
      (goto-char (point-min))
      (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
		 (re-search-forward "\
\[\t\n ]*<!-+[\t\n ]*Creative[\t\n ]+for[\t\n ]+"
				    nil t))
	(goto-char (match-beginning 0))
	(insert "\n<!-- End of Kiji -->\n")))
     ((string-equal group "digital")
      (shimbun-remove-tags "\
\[\t\n ]*<![\t\n ]*-+[\t\n ]*[$B!z!y(B]+[\t\n ]*AD[\t\n ]*[$B!z!y(B]+[\t\n ]*-+>"
			   "\
<![\t\n ]*-+[\t\n ]*/[\t\n ]*[$B!z!y(B]+[\t\n ]*AD[\t\n ]*[$B!z!y(B]+[\t\n ]*-+>\
\[\t\n ]*")
      (cond ((string-match "$B%3%i%`(B" from)
	     (unless (re-search-forward (shimbun-content-end shimbun) nil t)
	       (when (re-search-forward "\\(?:[\t\b ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<img[\t\n ]+src=\"[^>]*[\t\n ]*alt=\"$B%W%m%U%#!<%k(B\"\
\\|<h[0-9]>$B%W%m%U%#!<%k(B</h[0-9]>\\)"
					nil t)
		 (goto-char (match-beginning 0))
		 (insert "\n<!-- End of Kiji -->\n"))))))
     ((string-equal group "editorial")
      (let ((url (shimbun-header-xref header))
	    (retry 0)
	    start)
	(while retry
	  (if (progn
		(while (and (re-search-forward "\
<\\(h[0-9]+\\)[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"bdr_btm_2px"
					       nil t)
			    (shimbun-end-of-tag (match-string 1) t)
			    (if start
				(progn
				  (delete-region
				   (goto-char (match-beginning 0))
				   (match-beginning 1))
				  (insert "\n&#012;\n")
				  (forward-char 1))
			      (setq start (match-beginning 1)))))
		(and start
		     (re-search-forward "[\t\n ]*\
\\(?:<ul[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"FollowLnk\
\\|<li[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"TopLnk\"\
\\|<dl[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"PrInfo\"\\)"
					nil t)))
	      (progn
		(goto-char (match-beginning 0))
		(setq retry nil)
		(insert "\n<!-- End of Kiji -->")
		(goto-char start)
		(insert "<!-- Start of Kiji -->")
		(when (string-match "/editorial\\.html\\'" url)
		  (insert "\
\n<p>($B;XDj$5$l$?(B&nbsp;url&nbsp;$B$,(B&nbsp;$B$^$@(B/$B$9$G$K(B&nbsp;$BL5$$$N$G!"(B\
<a href=\"" url "\">$B%H%C%W%Z!<%8(B</a> $B$+$i5-;v$r<hF@$7$^$7$?(B)</p>\n")))
	    (erase-buffer)
	    (if (= retry 1)
		(setq retry nil)
	      (setq url "http://www.asahi.com/paper/editorial.html"
		    retry 1
		    start nil)
	      (shimbun-header-set-xref header url)
	      (shimbun-fetch-url shimbun url)
	      (goto-char (point-min)))))))
     ((string-equal group "food")
      (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
		 (re-search-forward "[\t\n ]*<!-+[\t\n ]+Creative[\t\n ]+for"
				    nil t))
	(goto-char (match-beginning 0))
	(insert "\n<!-- End of Kiji -->\n")))
     ((string-equal group "housing")
      (shimbun-remove-tags
       "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--$B9-9p%9%-%C%W(B -->"
       "<!--/$B9-9p%9%-%C%W$N$H$S@h(B-->[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*")
      (when (string-match "$B%^%s%7%g%s(B\\|$B8M7z$F(B" from)
	(goto-char (point-min))
	(re-search-forward "<td[\t\n ]+valign=\"top\">[\t\n ]*\
\\(?:<[^>]+>[\t\n ]*\\)*"
			   nil t)
	(insert "<!-- Start of Kiji -->")
	(when (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*</td>"
				 nil 'move)
	  (goto-char (match-beginning 0))
	  (insert "<!-- End of Kiji -->"))))
     ((string-equal group "rss"))
     ((string-equal group "tenjin")
      (let ((url (shimbun-header-xref header))
	    (retry 0)
	    start)
	(while retry
	  (if (and (re-search-forward "\
<p>20[0-9][0-9]$BG/(B[01]?[0-9]$B7n(B[0-3]?[0-9]$BF|!J(B[$BF|7n2P?eLZ6bEZ(B]$B!KIU(B</p>\
\\(\\(?:[\t\n ]*$B0u:~(B\\)?[\t\n ]*<[^>]+>\\)+[\t\n ]*"
				      nil t)
		   (progn
		     (setq start (match-end 0))
		     (re-search-forward "[\t\n ]*</p>" nil t)))
	      (save-restriction
		(narrow-to-region start (goto-char (match-beginning 0)))
		(setq retry nil)
		(insert "</p>\n<!-- End of Kiji -->")
		(goto-char start)
		(insert "<!-- Start of Kiji --><p>")
		(when (string-match "/column\\.html\\'" url)
		  (insert "\
\n<p>($B;XDj$5$l$?(B&nbsp;url&nbsp;$B$,(B&nbsp;$B$^$@(B/$B$9$G$K(B&nbsp;$BL5$$$N$G!"(B\
<a href=\"" url "\">$B%H%C%W%Z!<%8(B</a> $B$+$i5-;v$r<hF@$7$^$7$?(B)</p>\n"))
		(while (re-search-forward
			(eval-when-compile
			  (concat "[$B"'(B"
				  (condition-case nil
				      (list (make-char 'mule-unicode-2500-33ff
						       33 124))
				    (error nil))
				  "]"))
			nil t)
		  (replace-match "$B!#(B</p>\n<p>$B!!(B")))
	    (erase-buffer)
	    (if (= retry 1)
		(setq retry nil)
	      (setq url "http://www.asahi.com/paper/column.html"
		    retry 1
		    start nil)
	      (shimbun-header-set-xref header url)
	      (shimbun-fetch-url shimbun url)
	      (goto-char (point-min)))))))
     ((string-match "\\`book\\(?:\\.\\|\\'\\)" group)
      (while (re-search-forward "\\(<a[\t\n ]+[^>]+>\\)\
\[\t\n ]*<img[\t\n ]+[^>]+>[\t\n ]*</a>"
				nil t)
	(when (save-match-data
		(search-backward "alt=\"No image\"" (match-beginning 0) t))
	  (replace-match "\\1No image</a>"))))
     ((string-match "\\`shopping\\." group)
      (when (re-search-forward "\
<!-+[\t\n ]*end[\t\n ]+of[\t\n ]+headline[\t\n ]*-+>[\t\n ]*\
<p[\t\n ]+class=[^>]+>[\t\n ]*\\([^<]+\\)</p>[\t\n ]*\
<p[\t\n ]+id=\"date\">[\t\n ]*20[0-9][0-9]$BG/(B[01]?[0-9]$B7n(B[0-3]*[0-9]$BF|(B[\t\n ]*\
</p>[\t\n ]*"
			       nil t)
	(replace-match "<!-- Start of Kiji -->\\1<br>\n")))
     ((string-match "$B$N$N$A$c$s$N#D#O2J3X(B" from)
      ;; Remove furigana.
      (while (re-search-forward "\\(\\cj\\)$B!J(B\\cH+$B!K(B" nil t)
	(replace-match "\\1")))
     ((string-match "$B$f$k$f$k%U%'%_%K%s(B" from)
      (let (comics)
	(while (re-search-forward
		"<img[\t\n ]+src=\"[^>]+alt=\"$B%^%s%,(B\"[^>]*>"
		nil t)
	  (push (match-string 0) comics))
	(erase-buffer)
	(when comics
	  (insert "<!-- Start of Kiji -->\n"
		  (mapconcat 'identity comics "<br>\n")
		  "\n<!-- End of Kiji -->\n"))))
     ((string-match "$BA%66MN0l$N@$3&%V%j!<%U%#%s%0(B" from)
      (when (re-search-forward "\
<img[\t\n ]+src=\"[^>]+[\t\n ]+alt=\"$BA%66MN0l4i<L??(B\">"
			       nil t)
	(goto-char (match-beginning 0))
	(insert "<!-- Start of Kiji -->")
	(when (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<TD[\t\n ]+id=\"sidebar\">\
\\|<a[\t\n ]+href=\"http://opendoors\\.asahi\\.com/data/detail/\
\\|<!-+[\t\n ]*$B%H%T%C%/%9(B[\t\n ]*-+>\\)"
				 nil t)
	  (goto-char (match-beginning 0))
	  (insert "\n<!-- End of Kiji -->"))))
     ((string-match "$BCf9qFC=8(B" from)
      (let (start)
	(when (and (re-search-forward "\
<H2>$BCf9q:G?7%K%e!<%9(B</H2>[\t\n ]*<H1>[^>]+</H1>[\t\n ]*"
				      nil t)
		   (progn
		     (setq start (match-end 0))
		     (re-search-forward "\
<p[^>]*>[\t\n ]*([01][0-9]/[0-3][0-9])[\t\n ]*</p>"
					nil t)))
	  (delete-region (match-end 0) (point-max))
	  (insert "\n<!-- End of Kiji -->")
	  (delete-region (point-min) (goto-char start))
	  (insert "<!-- Start of Kiji -->\n"))))
     ((string-match "$BElMN7P:Q%K%e!<%9(B" from)
      ;; Insert newlines.
      (shimbun-with-narrowed-article
       shimbun
       (while (re-search-forward "$B!#!!(B?\\(\\cj\\)" nil t)
	 (replace-match "$B!#(B<br><br>$B!!(B\\1"))))
     ((string-match "$BF#Bt<~J?$N@$3&(B\\|$B?M4V9qJu(B" from)
      (when (re-search-forward "\
<div[\t\n ]+\\(?:class=\"kiji\"\\|id=\"kokuhou-waza\"\\)>[\t\n ]*"
			       nil t)
	(insert "\n<!-- Start of Kiji -->\n")
	(when (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)?\\(?:[\t\n ]*20[0-9][0-9]$BG/(B[01]?[0-9]$B7n(B[0-3]?[0-9]$BF|(B\
\\(?:[\t\n ]*<[^>]+>\\)*\\)?[\t\n ]*<!-+[\t\n ]*google"
				 nil t)
	  (goto-char (match-beginning 0))
	  (insert "\n<!-- End of Kiji -->\n")))))
    (shimbun-with-narrowed-article
     shimbun
     ;; Remove sitesearch area.
     (when (re-search-forward "[\t\n ]*\\(?:<div[\t\n ]+[^>]+>[\t\n ]*\\)+\
$B$3$N5-;v$N4XO">pJs$r%"%5%R!&%3%`Fb$G8!:w$9$k(B"
			      nil t)
       (goto-char (match-beginning 0))
       (insert "\n<!-- End of Kiji -->"))
     ;; Remove ads.
     (goto-char (point-min))
     (when (re-search-forward "[\t\n ]*<p[\t\n ]+class=\"hide\">[\t\n ]\
*$B$3$3$+$i9-9p$G$9(B[\t\n ]*</p>"
			      nil t)
       (let ((start (match-beginning 0)))
	 (when (re-search-forward "<p[\t\n ]+class=\"hide\">[\t\n ]*\
$B9-9p=*$o$j(B\\(?:[\t\n ]*</p>[\t\n ]*\\|\\'\\)"
				  nil t)
	   (delete-region start (match-end 0)))))
     ;; Remove trailing garbage.
     (goto-char (point-min))
     (when (and (not (string-match "$B$f$k$f$k%U%'%_%K%s(B" from))
		(re-search-forward
		 "\\(?:</p>\\)?\\(\\(?:[\t\n $B!!(B]*<[^>]+>\\)+[\t\n $B!!(B]*\\'\\)"
		 nil t))
       (goto-char (match-beginning 0))
       (while (or (and (looking-at "[\t\n $B!!(B]*\\(</[^>]+>\\)[\t\n $B!!(B]*")
		       ;; Don't remove close tags.
		       (progn (replace-match "\\1") t))
		  (and (looking-at "[\t\n $B!!(B]*<[^>]+>[\t\n $B!!(B]*\\|[\t\n $B!!(B]+")
		       (progn (replace-match "") t))))
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-asahi)
						   header)
  (shimbun-asahi-prepare-article shimbun header))

(defun shimbun-asahi-clear-contents (shimbun header)
  (when (luna-call-next-method)
    ;; Remove table tags that surround image tags.
    (goto-char (point-min))
    (let (end start found images)
      (while (re-search-forward "[\t\n ]*<table[\t\n ]+[^>]+>[\t\n ]*\
\\(?:\\(?:<[^>]+>[\t\n ]*\\)*\
<img[\t\n ]+[^>]+>[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*[^<]+\\)+\
\\(?:<[^>]+>[\t\n ]*\\)*</table>[\t\n ]*"
				nil t)
	(setq found nil
	      images nil
	      end (match-end 0))
	(goto-char (setq start (match-beginning 0)))
	(while (re-search-forward "\
\\(<img[\t\n ]+[^>]+>\\)[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\\([^<]+\\)"
				  end t)
	  (skip-chars-backward "\t\n ")
	  (when (> (point) (match-beginning 2))
	    (setq found t))
	  (push (concat (match-string 1) "<br>"
			(buffer-substring (match-beginning 2) (point)))
		images)))
      (when found
	(setq images (nreverse images))
	(delete-region start end)
	(insert "\n")
	(while images
	  (insert (pop images))
	  (insert (if images "<br><br>\n" "\n")))))
    ;; Remove zoom buttons.
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*<img\\(?:[\t\n ]+[^\t\n >]+\\)*\
\[\t\n ]+class=\"ThmbZoomBtn\"[^>]*>[\t\n ]*"
			      nil t)
      (replace-match "\n"))
    ;; Remove garbage before images.
    (goto-char (point-min))
    (while (re-search-forward
	    "\\(?:<\\(?:p\\|span\\)>[\t\n ]*\\)+\\(<img[\t\n ]+\\)"
	    nil t)
      (replace-match "\\1"))
    ;; Remove garbage after images.
    (goto-char (point-min))
    (while (re-search-forward "\\(<img[\t\n ]+[^>]+>\\)[\t\n $B!!(B]*\
\\(\\(?:<![^>]+>\\|<br>\\)[\t\n $B!!(B]*\\)*<p>"
			      nil t)
      (replace-match "\\1\n<p>"))
    ;; Add line breaks after images that captions or images follow.
    (goto-char (point-min))
    (while (re-search-forward
	    "\\(<img[\t\n ]+[^>]+>\\(?:[\t\n ]*</[^>]+>\\)*\\)[\t\n ]*"
	    nil t)
      (when (or (save-match-data
		  (looking-at "\\(?:<[^\t\n >]+>[\t\n ]*\\)*<img[\t\n ]\
\\|<small>[^<]+</small>"))
		(not (eq (char-after) ?<)))
	(replace-match "\\1<br>\n")))
    ;; Add line breaks before images that follow captions.
    (goto-char (point-min))
    (while (re-search-forward
	    "[\t\n ]*\\(\\(?:<[^/>][^>]*>[\t\n ]*\\)*<img[\t\n ]\\)"
	    nil t)
      (unless (memq (char-before (match-beginning 0)) '(nil ?>))
	(replace-match "<br>\n\\1")))
    ;; Remove related topics.
    (let (start)
      (goto-char (point-min))
      (while (and (re-search-forward "\\(\\(?:[\t\n ]*</div>\\)*[\t\n ]*\\)\
<div[\t\n ]+[^>]+>\\(?:[\t\n ]*<h[0-9]+>\\)?[\t\n ]*$B4XO"%H%T%C%/%9(B[\t\n ]*<"
				     nil t)
		  (progn
		    (setq start (match-beginning 0))
		    (goto-char (match-end 1))
		    (shimbun-end-of-tag "div" t)))
	(delete-region start (match-end 0))
	(insert "\n"))
      (goto-char (point-min))
      (while (re-search-forward
	      "[\t\n ]*<h2>[\t ]*$B$3$s$J5-;v$b(B[\t ]*</h2>[\t\n ]*" nil t)
	(setq start (match-beginning 0))
	(while (and (looking-at "\
\\(</div>[\t\n ]*\\)?<ul[\t\n ]+class=\"\\(?:Follow\\)*Lnk\"")
		    (progn
		      (when (match-end 1) (goto-char (match-end 1)))
		      (shimbun-end-of-tag "ul" t))))
	(delete-region start (point))))

    ;; Remove blogs link.
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*\\(?:<[^/][^>]+>[\t\n ]*\\)+\
$B$3$N5-;v$rMxMQ$7$?%V%m%00lMw(B\\(?:[\t\n ]*<[!/][^>]+>\\)+[\t\n ]*"
			      nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Remove form, noscript, and script tags.
    (shimbun-remove-tags "form\\|noscript\\|script" t)
    ;; Remove empty tables.
    (goto-char (point-min))
    (let (start end limit found)
      (while (and (re-search-forward "<table[\t\n >]" nil t)
		  (shimbun-end-of-tag "table" t))
	(setq start (match-beginning 0)
	      end (match-end 0)
	      limit (match-end 3))
	(goto-char (match-beginning 3))
	(while (and (not found)
		    (re-search-forward "<[\t\n ]*\\([^\t\n >]+\\)" limit t))
	  (unless (string-match "\\`\\(?:!\\|/?t[dr]\\'\\)" (match-string 1))
	    (setq found t)))
	(if found
	    (goto-char end)
	  (delete-region start end)
	  (insert "\n"))))

    ;; Remove any other useless things.
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*\
\\(?:<div[\t\n ]+[^>]+>\\|</div>\\|<ul>[\t\n ]*</ul>\\)\
\[\t\n ]*"
			      nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*\\(?:<[^/][^>]*>[\t\n ]*\\)+\
\\(?:$B%"%5%R!&%3%`(B\\|$B%K%e!<%9(B\\)$B%H%C%W(B[$B$X%X(B]\
\\(?:\\(?:[\t\n ]*<[!/][^>]+>\\)+[\t\n ]*\\|[\t\n ]*\\'\\)"
			      nil t)
      (replace-match "\n")
      (backward-char 1))

    (shimbun-remove-orphaned-tag-strips "span\\|p")

    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-asahi)
						    header)
  (shimbun-asahi-clear-contents shimbun header))

(defun shimbun-asahi-multi-clear-contents (shimbun header
						   has-previous-page
						   has-next-page)
  (when (luna-call-next-method)
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n")
      (when (looking-at "[\t\n ]*<p>[\t\n ]*")
	(delete-region (match-beginning 0) (match-end 0))))
    t))

(luna-define-method shimbun-multi-clear-contents :around ((shimbun
							   shimbun-asahi)
							  header
							  has-previous-page
							  has-next-page)
  (shimbun-asahi-multi-clear-contents shimbun header
				      has-previous-page has-next-page))

(provide 'sb-asahi)

;;; sb-asahi.el ends here
