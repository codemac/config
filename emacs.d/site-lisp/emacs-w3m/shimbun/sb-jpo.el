;;; sb-jpo.el --- shimbun backend for http://www.jpo.go.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Apr. 28, 2003

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
;;
;; This shimbun backend categorizes the Japan Patent Office web site
;; to virtual four news groups.  Following is table of a site map
;; (summaerized page of http://www.jpo.go.jp/sitemap/index.htm) and
;; mapped virtual groups.
;;
;; $B"#@)EY$N>R2p(B                          ...lawguide
;; $B"#=P4j$+$i?3::!"?3H=!"EPO?$^$G(B        ...details
;; $B"#FC5vD#$N>R2p(B
;; $B"#FC5vD#$N<h$jAH$_(B
;;    $B"#%W%l%9H/I=(B                       ...news
;;    $B"#K!Na2~@5$N$*CN$i$;(B               ...revision
;;    $B"#9-Js$N9->l(B                       ...news
;;    $B"#(B...                              ...details
;;
;; $B"#;qNA<<(B                              ...details
;; $B"#$*Ld$$9g$o$;(B
;; $B"#99?7MzNr(B
;; $B"#%/%$%C%/%,%$%I(B
;; $B"#D4C#>pJs!&8xJg>pJs(B
;; $B"#0U8+Jg=8!'%Q%V%j%C%/!&%3%a%s%H(B
;; $B"#FC5vEE;R?^=q4[(B
;; $B"#FHN)9T@/K!?M!!9)6H=jM-8"Am9g>pJs4[(B
;; $B"#4XO"%[!<%`%Z!<%8%j%s%/(B
;; $B"#5lFC5vD#%[!<%`%Z!<%8(B
;; $B"#EE;R@/I\$NAm9gAk8}!JAmL3>J9T@/4IM}6I!K(B
;; $B"#$3$N%5%$%H$K$D$$$F(B

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-jpo (shimbun) ())

(defconst shimbun-jpo-url "http://www.jpo.go.jp/")
(defvar shimbun-jpo-groups
  '("news" ;$B"#%W%l%9H/I=(B   $B"#9-Js$N9->l(B
    "revision" ;$B"#K!Na2~@5$N$*CN$i$;(B
    "lawguide" ;$B"#@)EY$N>R2p(B
    "details" ; $B"#=P4j$+$i?3::!"?3H=!"EPO?$^$G(B  $B"#FC5vD#$N<h$jAH$_(B  $B"#;qNA<<(B
    ))
(defvar shimbun-jpo-from-address "webmaster@jpo.go.jp")
(defvar shimbun-jpo-coding-system 'japanese-shift-jis)
(defvar shimbun-jpo-content-start "<body [^\n]+>")
(defvar shimbun-jpo-content-end "<\/body>")

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-jpo))
;;  (shimbun-from-address-internal shimbun))
(defvar shimbun-jpo-debugging t)

(defun shimbun-jpo-retrieve-url (url &optional no-cache no-decode)
  (if shimbun-jpo-debugging
      (let (w3m-async-exec)
	(shimbun-retrieve-url url no-cache no-decode))
    (shimbun-retrieve-url url no-cache no-decode)))

(luna-define-method shimbun-headers ((shimbun shimbun-jpo) &optional range)
  (shimbun-jpo-headers shimbun))

(defun shimbun-jpo-headers (shimbun)
  (let ((group (shimbun-current-group-internal shimbun))
	(url shimbun-jpo-url)
	headers)
    (with-temp-buffer
      (if (string= group "news")
	  (progn
	    (setq url (concat url "torikumi/torikumi_list.htm"))
	    (shimbun-jpo-retrieve-url url 'reload)
	    (setq headers (shimbun-jpo-headers-1
			   shimbun url "\\(hiroba/.*\\)"))
	    (erase-buffer)
	    (setq url (concat shimbun-jpo-url
			      "torikumi/puresu/puresu_list.htm"))
	    (shimbun-jpo-retrieve-url url 'reload)
	    (setq headers (nconc headers (shimbun-jpo-headers-1 shimbun url))))
	(if (string= group "details")
	    (setq headers (shimbun-jpo-headers-group-details shimbun))
	  (if (string= group "revision")
	      (setq url (concat url "torikumi/kaisei/kaisei2/kaisei_list.htm"))
	    (if (string= group "lawguide")
		(setq url (concat url "seido/seido_list.htm"))
	      (error "unknown group %s" group)))
	  (shimbun-jpo-retrieve-url url 'reload)
	  (setq headers (shimbun-jpo-headers-1 shimbun url)))))
    headers))

(defun shimbun-jpo-headers-1 (shimbun origurl &optional urlregexp unmatchregexp)
  (let ((case-fold-search t)
	(from (shimbun-from-address shimbun))
	(group (shimbun-current-group-internal shimbun))
	(regexp (format "<td><font color=\"[#0-9A-Z]+\"><a href=\"\\(%s\\.html*\\)\">\\(.*\\)</a>[$B!!(B ]*\\([.0-9]+\\)" (or urlregexp "\\(.*\\)")))
	(urlprefix
	 (when (string-match "^\\(http:\/\/.+\\/\\)[^\/]+\\.html*" origurl)
	   (match-string 1 origurl)))
	headers id pagename subject tempdate date url)
    ;; <td><font color="#2346AB"><a href="h1504_pat_kijitu.htm">$BFC5vK!Ey$N0lIt$r2~@5$9$kK!N'$N0lIt$N;\9T4|F|$rDj$a$k@/Na0F$K$D$$$F(B</a>$B!!(B2003.4.21</font></td>
    ;; getting URL and SUBJECT
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (catch 'next
	(setq pagename (match-string-no-properties 1)
	      subject (match-string-no-properties 3)
	      date (match-string-no-properties 4))
	(when (and unmatchregexp (string-match unmatchregexp pagename))
	  (throw 'next nil))
	(setq url (shimbun-expand-url pagename urlprefix)
	      subject (with-temp-buffer
			  (insert subject)
			  (shimbun-remove-markup)
			  (buffer-string)))
	;; getting DATE
	(if (not (string-match
		  "\\([0-9]+\\)\\.\\([0-9]+\\)\\(\\.[0-9]+\\)?" date))
	    (throw 'next nil) ; unknown date format
	  (setq tempdate (list (string-to-number (match-string 1 date))
			       (string-to-number (match-string 2 date))))
	  (setq date (nconc tempdate 
			    (list
			     (if (not (match-string 3 date))
				1
			       (string-to-number
				(substring (match-string 3 date) 1)))))))
	;; building ID
	(setq id (format
		  "<%04d%02d%02d%%%s%%%s@jpo>"
		  (car date) (nth 1 date) (nth 2 date)
		  (if (string-match "^http:\/\/.+\\/\\([^\/]+\\.html*\\)"
				    pagename)
		      (match-string 1 pagename)
		    pagename)
		  group))
	(unless (shimbun-search-id shimbun id)
	  (setq date (apply 'shimbun-make-date-string date))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 from date id "" 0 0 url)
		headers))
	(forward-line 1)))
    headers))

(defun shimbun-jpo-headers-group-details (shimbun)
  (let ((case-fold-search t)
	(urllist '("torikumi/torikumi_list.htm"
		   "tetuzuki/tetuzuki_list.htm"
		   "shiryou/shiryou_list.htm"))
	(exceptions-alist
	 (list (cons "torikumi/" (list "kaisei/" "puresu/" "hiroba/"))))
	url headers pages urlprefix temp exceptions)
    (while urllist
      (when (string-match "\\/" (car urllist))
	(setq urlprefix (substring (car urllist) 0 (1+ (match-beginning 0)))))
      (setq url (shimbun-expand-url (car urllist) shimbun-jpo-url))
      (erase-buffer)
      (shimbun-jpo-retrieve-url url 'reload)
      (setq exceptions (cdr (assoc urlprefix exceptions-alist)))
      (goto-char (point-min))
      ;; gathering header information of articles in the current page.
      (setq headers (nconc headers
			   (shimbun-jpo-headers-1
			    shimbun url nil
			    (when exceptions
			      (concat "\\(" 
				      (mapconcat 'regexp-quote exceptions "\\|")
				      "\\)")))))
      (goto-char (point-min))
      (while (re-search-forward
	      ;;<td><font color="#2346AB"><a href="puresu/puresu_list.htm">$B%W%l%9H/I=(B</a></font></td>
	      "<td><font color=\"[#0-9A-Z]+\"><a href=\"\\(.*\\.htm\\)\">[^<>]+<\/a><\/font><\/td>"
	      nil t nil)
	;; getting sub-categories.
	(catch 'next
	  (setq temp (match-string 1))
	  (dolist (ex exceptions)
	    (when (string-match ex temp)
	      (throw 'next nil)))
	  (setq pages (cons (shimbun-expand-url
			     temp
			     (concat shimbun-jpo-url urlprefix)) pages))))
      (while pages
	(setq url (car pages))
	(erase-buffer)
	(shimbun-jpo-retrieve-url url 'reload)
	;; getting header information of articles in pages of the sub-categories.
	(setq headers (nconc
		       headers
		       (shimbun-jpo-headers-1 shimbun url)))
	(setq pages (cdr pages)))
      (setq urllist (cdr urllist)))
    headers))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-jpo)
						    header)
  (when (luna-call-next-method)
    (goto-char (point-min))
    (when (re-search-forward
	   ;;$B#E!](Bmail$B!'(B<a href="mailto:PA0A00@jpo.go.jp">PA0A00@jpo.go.jp<br>
	   ;;E-mail$B!'(B<a href="mailto:PA0420@jpo.go.jp">PA0420@jpo.go.jp</a></font>
	   ;;$B!!(BE-mail:<a href="mailto:PA0A00@jpo.go.jp"> PA0A00@jpo.go.jp</a></font>
	   "\\($BEE;R%a!<%k(B\\|[$B#E#e(BEe][$B!](B-]*[$B#M#m(BMm][$B#A#a(BAa][$B#I#i(BIi][$B#L#l(BLl]\\)[$B!'(B:$B!!(B] *<a href=\"mailto:\\(.*@.*jpo.go.jp\\)\"> *\\2"
	   nil t nil)
      (shimbun-header-set-from header (match-string 2)))
    (shimbun-jpo-cleanup-article)
    t))

(defun shimbun-jpo-cleanup-article ()
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      ;; <td align="center"><a href="#top"><img src="images/gotop.gif" width="89" height="13" border="0" vspace="10" alt="$B%Z!<%8$N@hF,$X(B"></a></td>
      (while (re-search-forward
	      "<img src=\"images/gotop.gif\" .*alt=\"$B%Z!<%8$N@hF,$X(B\">"
	      nil t nil)
	(delete-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
      (goto-char (point-min))
      ;; <td align="left"><a href="../../../indexj.htm" target="_top">HOME</a> &gt; <a href="../../torikumi_list.htm">$BFC5vD#$N<h$jAH$_!JFC5vK!Bh#3#0>rEy?75,@-$NAS<:$NNc30!K$NE,MQ$K4X$7$F!K(B</a> &gt;<br><br></td>
      ;; <td align="left"><a href="../../indexj.htm" target="_top">HOME</a> &gt; <a href="../torikumi_list.htm">$BFC5vD#(B
      (while (re-search-forward
	      "<td align=\"left\"><a href=\"\\(\\.\\./\\)+indexj.htm\" target=\"_top\">HOME<\/a> *\\&gt;"
	      nil t nil)
	(delete-region (match-beginning 0) (progn (end-of-line) (point))))
      (goto-char (point-min))
      (while (re-search-forward 
	      "<tr>\n+<td align=\"left\"><img src=\"\\(\\.\\./\\)?images/title\\.gif\" *[^<>]+\">\\(<\/a>\\)?<\/td>\n+<\/tr>"
	      nil t nil)
	(delete-region (match-beginning 0) (match-end 0)))
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\(<td>\\)?<a href=\"http://www.adobe.co.jp/products/acrobat/readstep.html"
	      nil t nil)
	(delete-region (match-beginning 0) (progn (end-of-line) (point))))
      (goto-char (point-min))
      (while (re-search-forward
	      ;; PDF$B%U%!%$%k$r=i$a$F$*;H$$$K$J$kJ}$O!"(BAdobe Acrobat Reader$B%@%&%s%m!<%I%Z!<%8$X(B   
	      "Adobe Acrobat Reader *$B%@%&%s%m!<%I%Z!<%8(B"
	      nil t nil)
	(delete-region (progn (beginning-of-line) (point)) (progn (end-of-line) (point))))
      (goto-char (point-min))
      (while (re-search-forward 
	      "<tr>\n+<td align=\"center\"><a href=\"#top\">\
<img src=\"\\(\\.\\.\/\\)?images/gotop\\.gif\" [^<>]+\">\\(<\/a>\\)?<\/td>\n+<\/tr>"
	      nil t nil)
	(delete-region (match-beginning 0) (match-end 0))))))

(provide 'sb-jpo)

;;; sb-jpo.el ends here
