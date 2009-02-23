;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bloggin
;; This should create a new file with the next numerical value
;; and add some boilerplate

(defun blog-insert-meta ()
  (interactive)
  (insert "[[!meta title=\"\"]]\n")
  (insert "[[!tag ]]\n")
  (insert "\n")
)

(defun blog-last ()
  (interactive)
  (let ((wiki-dir "~/www/wiki/blog/"))
    (find-file
     (concat wiki-dir
	     (number-to-string (apply 'max (mapcar 'string-to-number
	     (mapcar '(lambda (a) (substring a 0 -5))
		     (directory-files wiki-dir nil "[0-9]*\\.mdwn" t )))))
	     ".mdwn")))
)

(defun blog-find-next ()
  (interactive)
  (let ((wiki-dir "~/www/wiki/blog/"))
    (find-file 
     (concat wiki-dir 
	     (number-to-string (1+ (apply 'max (mapcar 'string-to-number 
	     (mapcar '(lambda (a) (substring a 0 -5)) 
					 (directory-files wiki-dir nil "[0-9]*\\.mdwn" t))))))
	     ".mdwn")))
)

(defun blog-next ()
  (interactive)
  (blog-find-next)
  (end-of-buffer)
  (blog-insert-meta)
)

;;

(provide 'cm-blog)