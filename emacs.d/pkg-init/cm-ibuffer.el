;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yay ibuffer
;;;
(require 'ibuffer)

;; replace emac's default buffer list with the excellent ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-ibuffer-sorter filename-or-dired
  "Sort the buffers by their pathname."
  (:description "filenames plus dired")
  (string-lessp 
   (with-current-buffer (car a)
     (or buffer-file-name
	 (if (eq major-mode 'dired-mode)
	     (expand-file-name dired-directory))
	 ;; so that all non pathnames are at the end
	 "~"))
   (with-current-buffer (car b)
     (or buffer-file-name
	 (if (eq major-mode 'dired-mode)
	     (expand-file-name dired-directory))
	 ;; so that all non pathnames are at the end
	 "~"))))

;; Add pathnam sorting, useful after 's m'
(define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-filename-or-dired)

(provide 'cm-ibuffer)