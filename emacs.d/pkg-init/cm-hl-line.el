;; From emacs-wiki:
(defun shade-color (intensity)
  "print the #rgb color of the background, dimmed according to intensity"
  (interactive "nIntensity of the shade : ")
  (apply 'format "#%02x%02x%02x"
         (mapcar (lambda (x)
                   (if (> (lsh x -8) intensity)
                       (- (lsh x -8) intensity)
                     0))
                 (color-values (cdr (assoc 'background-color (frame-parameters)))))))

;; Default hl
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)
;(set-face-background hl-line-face (shade-color 08))  

;(defface hl-line-highlight-face
;  '((t :inherit highlight))
;  "Face for highlighting the current line with `hl-line-fancy-highlight'."
;  :group 'hl-line)

;(defun hl-line-fancy-highlight ()
;  (set (make-local-variable 'hl-line-face) 'hl-line-highlight-face)
;  ;;    (set (make-local-variable 'line-move-visual) nil)
;  ;;    (set (make-local-variable 'cursor-type) nil)
;  (setq global-hl-line-mode nil)
;  (hl-line-mode))

;(add-hook 'org-agenda-mode-hook 'hl-line-fancy-highlight)
;(add-hook 'gnus-summary-mode-hook 'hl-line-fancy-highlight)
;(add-hook 'gnus-group-mode-hook 'hl-line-fancy-highlight)

(provide 'cm-hl-line)
