 ;; Author:Tatsuhiko Kubo
 ;; This elisp can keeping in touch between header file and source file for C or C++
 
 ;; This program is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2, or (at your option)
 ;; any later version.
 
 
 (defun c-open-relational-file-get-opened-file-name-prefix (file-name)
   (string-match "/\\([^./]+\\)\\.[^.]+$" file-name)
   (match-string 1 file-name))
 
 (defun c-open-relational-file-get-ext-type (file-name)
   (string-match "\\.\\([^.]+\\)$" file-name)
   (match-string 1 file-name))
 
 (defun c-open-relational-file-get-opening-file-name (file-name-prefix ext-list)
   (let ((opening-file-name (concat file-name-prefix "." (car ext-list))))
     (cond ((null (car ext-list))             nil)
 	  ((file-exists-p opening-file-name) opening-file-name)
 	  (t                                 (c-open-relational-file-get-opening-file-name file-name-prefix 
 											   (cdr ext-list))))))
 
 (defun c-open-relational-file (how-open-type)
   "keeping in touch between header file and source file for C or C++"
   (interactive "nOpen-Type 1:elscreen,2:split; ")
   (let* ((c-or-cpp-header-map (list "c" "cpp" "cxx" "cc" "c++" "C"))
 	 (c-source-map        (list "h" "s"))
 	 (asm-source-map      (list "c"))
 	 (cpp-source-map      (list "hpp" "h" "hxx" "h++" "hh" "H"))
 	 (cpp-header-map      (list "cpp" "cxx" "cc" "c++" "C"))
 	 (ext-map (list
 		   (cons "h"   c-or-cpp-header-map)
 		   (cons "c"   c-source-map)
 		   (cons "s"   asm-source-map)
 		   (cons "C"   cpp-source-map)
 		   (cons "cc"  cpp-source-map)
 		   (cons "cpp" cpp-source-map)
 		   (cons "cxx" cpp-source-map)
 		   (cons "c++" cpp-source-map)
 		   (cons "H"   cpp-header-map)
 		   (cons "hh"  cpp-header-map)
 		   (cons "hpp" cpp-header-map)
 		   (cons "hxx" cpp-header-map)
 		   (cons "h++" cpp-header-map)))
 	 (opened-file-name (buffer-file-name (window-buffer)))
 	 (opened-file-name-prefix (c-open-relational-file-get-opened-file-name-prefix opened-file-name))
 	 (opened-file-ext-type (c-open-relational-file-get-ext-type opened-file-name))
 	 (opening-file-ext-type-list (cdr (assoc opened-file-ext-type ext-map)))
 	 (opening-file-name (c-open-relational-file-get-opening-file-name opened-file-name-prefix
 									  opening-file-ext-type-list))
 	 (opening-file-buffer (if (null opening-file-name)
 				  nil
 				(find-file-noselect opening-file-name))))
     (if (null opening-file-buffer)
 	(message "not found relational file")
       (cond ((= how-open-type 1) (switch-to-buffer opening-file-buffer))
 	    ((= how-open-type 2) (progn (split-window-horizontally)
 					(other-window 1)
 					(switch-to-buffer opening-file-buffer)))
 	    (t                   (message "Illegal Type"))))))