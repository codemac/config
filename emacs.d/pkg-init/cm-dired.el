;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired customization!

;; Have "^" and "Enter" open the next directory in the same
;; buffer.  I don't think there is a situation where I don't
;; want this to happen, so we'll roll with this.

;; reenable!
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "<return>")
    'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "a")
    'dired-advertised-find-file) ; was dired-find-alternate-file
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

(provide 'cm-dired)