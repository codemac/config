;;; sb-asahi-mytown.el --- mytown.asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-asahi-mytown
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-asahi-mytown-group-table
  '(("hokkaido" "$BKL3$F;(B" "0100000")
    ("aomori" "$B@D?9(B" "0200000")
    ("iwate" "$B4d<j(B" "0300000")
    ("miyagi" "$B5\>k(B" "0400000")
    ("akita" "$B=)ED(B" "0500000")
    ("yamagata" "$B;37A(B" "0600000")
    ("fukushima" "$BJ!Eg(B" "0700000")
    ("ibaraki" "$B0q>k(B" "0800000")
    ("tochigi" "$BFJLZ(B" "0900000")
    ("gunma" "$B72GO(B" "1000000")
    ("saitama" "$B:k6L(B" "1100000")
    ("chiba" "$B@iMU(B" "1200000")
    ("tokyo" "$BEl5~(B" "1300000")
    ("tama" "$BB?K`(B" "1400000")
    ("kanagawa" "$B?@F`@n(B" "1500000")
    ("niigata" "$B?73c(B" "1600000")
    ("toyama" "$BIY;3(B" "1700000")
    ("ishikawa" "$B@P@n(B" "1800000")
    ("fukui" "$BJ!0f(B" "1900000")
    ("yamanashi" "$B;3M|(B" "2000000")
    ("nagano" "$BD9Ln(B" "2100000")
    ("gifu" "$B4tIl(B" "2200000")
    ("shizuoka" "$B@E2,(B" "2300000")
    ("aichi" "$B0&CN(B" "2400000")
    ("mie" "$B;0=E(B" "2500000")
    ("shiga" "$B<"2l(B" "2600000")
    ("kyoto" "$B5~ET(B" "2700000")
    ("osaka" "$BBg:e(B" "2800000")
    ("hyogo" "$BJ<8K(B" "2900000")
    ("nara" "$BF`NI(B" "3000000")
    ("wakayama" "$BOB2N;3(B" "3100000")
    ("tottori" "$BD;<h(B" "3200000")
    ("shimane" "$BEg:,(B" "3300000")
    ("okayama" "$B2,;3(B" "3400000")
    ("hiroshima" "$B9-Eg(B" "3500000")
    ("yamaguchi" "$B;38}(B" "3600000")
    ("tokushima" "$BFAEg(B" "3700000")
    ("kagawa" "$B9a@n(B" "3800000")
    ("ehime" "$B0&I2(B" "3900000")
    ("kochi" "$B9bCN(B" "4000000")
    ("fukuoka" "$BJ!2,!&KL6e=#(B" "4100000")
    ("saga" "$B:42l(B" "4200000")
    ("nagasaki" "$BD9:j(B" "4300000")
    ("kumamoto" "$B7'K\(B" "4400000")
    ("oita" "$BBgJ,(B" "4500000")
    ("miyazaki" "$B5\:j(B" "4600000")
    ("kagoshima" "$B</;yEg(B" "4700000")
    ("okinawa" "$B2-Fl(B" "4800000"))
  "Alist of group names, their Japanese translations and ids.")

(defvar shimbun-asahi-mytown-server-name "$BD+F|?7J9(B")

(defvar shimbun-asahi-mytown-top-level-domain "mytown.asahi.com"
  "Name of the top level domain for the Mytown Asahi Shimbun.")

(defvar shimbun-asahi-mytown-url
  (concat "http://" shimbun-asahi-mytown-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-asahi-mytown-expiration-days 6)

(defvar shimbun-asahi-mytown-content-start
  "<!--[\t\n ]*Start of photo[\t\n ]*-->\\|<!--$B!z!z<L??$3$3$+$i!z!z(B-->\
\\|<!--[\t\n ]*Start of Kiji[\t\n ]*-->\\|<!--$B!z!zK\J8$3$3$+$i!z!z(B-->")

(defvar shimbun-asahi-mytown-content-end
  "<!--$B!z!zK\J8$3$3$^$G!z!z(B-->\\|<!--[\t\n ]*End of Kiji[\t\n ]*-->")

(defvar shimbun-asahi-mytown-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(luna-define-method shimbun-groups ((shimbun shimbun-asahi-mytown))
  (mapcar 'car shimbun-asahi-mytown-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi-mytown))
  (concat "$BD+F|%^%$%?%&%s(B ("
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			shimbun-asahi-mytown-group-table))
	  ")"))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi-mytown))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-asahi-mytown-url
	    group
	    "/newslist.php?d_id="
	    (nth 2 (assoc group shimbun-asahi-mytown-group-table)))))

(defun shimbun-asahi-mytown-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-current-group-name shimbun))
	(case-fold-search t)
	cyear cmonth url id subject month day year headers)
    (setq cyear (shimbun-decode-time nil 32400)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n $B!!(B]*")
		    (s1 "[\t\n ]+")
		    (no-nl "[^\n<>]+"))
		(concat
		 "<a" s1 "href=\""
		 ;; 1. url
		 "\\(news\\.php\\?k_id="
		 ;; 2. id
		 "\\([0-9]+\\)"
		 "\\)"
		 "\">" s0
		 ;; 3. subject
		 "\\(" no-nl "\\)"
		 s0 "</a>" s0 "\\(?:<[^>]+>" s0 "\\)(" s0
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 s0 "/" s0
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 s0 ")")))
	    nil t)
      (setq url (shimbun-expand-url (concat group "/" (match-string 1))
				    shimbun-asahi-mytown-url)
	    id (match-string 2)
	    subject (match-string 3)
	    month (string-to-number (match-string 4))
	    day (string-to-number (match-string 5))
	    year (cond ((>= (- month cmonth) 2)
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear)))
      (push (shimbun-create-header
	     ;; number
	     0
	     ;; subject
	     subject
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day)
	     ;; id
	     (format "<%d%02d%02d.%s%%%s.%s>"
		     year month day id group
		     shimbun-asahi-mytown-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     url)
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi-mytown)
					 &optional range)
  (shimbun-asahi-mytown-get-headers shimbun))

(defun shimbun-asahi-mytown-prepare-article ()
  "Remove trailing empty lines."
  (let ((case-fold-search t)
	end start)
    (when (and (re-search-forward shimbun-asahi-mytown-content-start nil t)
	       (re-search-forward shimbun-asahi-mytown-content-end nil t))
      (setq end (goto-char (match-beginning 0))
	    start end)
      (while (and (re-search-backward "[\t\n\r ]*\\(?:<[^>]+>[\t\n\r ]*\\)"
				      nil t)
		  (if (= (match-end 0) start)
		      (setq start (match-beginning 0))
		    (delete-region (goto-char start) end)
		    (insert "\n")
		    nil))))))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-asahi-mytown) header)
  (shimbun-asahi-mytown-prepare-article))

(provide 'sb-asahi-mytown)

;;; sb-asahi-mytown.el ends here
