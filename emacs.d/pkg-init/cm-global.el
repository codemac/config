;; gnu global!
(require 'gtags-autoloads)

(defun my-gtags-settings ()
  "Settings for gtags."

  ;; Key bindings.
  (define-prefix-command 'gtags-keymap)
  (define-key global-map (kbd "C-c g") 'gtags-keymap)

  (define-key gtags-mode-map (kbd "C->") 'gtags-find-tag-from-here)
  (define-key gtags-mode-map (kbd "C-<") 'gtags-pop-stack)
  (define-key gtags-mode-map (kbd "C-c g s") 'gtags-find-symbol)
  (define-key gtags-mode-map (kbd "C-c g t") 'gtags-find-tag)
  (define-key gtags-mode-map (kbd "C-c g r") 'gtags-find-rtag)
  (define-key gtags-mode-map (kbd "C-c g p") 'my-gtags-find-file)
  (define-key gtags-mode-map (kbd "C-c g v") 'gtags-visit-rootdir)
  (define-key gtags-mode-map [mouse-2] 'gtags-find-tag-by-event)
  (define-key gtags-mode-map [mouse-3] 'gtags-pop-stack)

  (define-key gtags-select-mode-map (kbd "n") 'next-line)
  (define-key gtags-select-mode-map (kbd "p") 'previous-line)
  (define-key gtags-select-mode-map (kbd "RET") 'gtags-select-tag)
  (define-key gtags-select-mode-map (kbd "C-<") 'gtags-pop-stack)
  (define-key gtags-select-mode-map (kbd "C->") 'gtags-select-tag)
  (define-key gtags-select-mode-map (kbd "q") 'gtags-pop-stack)
  (define-key gtags-select-mode-map [mouse-2] 'gtags-select-tag-by-event)
  (define-key gtags-select-mode-map [mouse-3] 'gtags-pop-stack)

  ;; Highlight gtags item line.
  (add-hook 'gtags-select-mode-hook '(lambda () (hl-line-mode 1)))

  ;; Update gtags data after save file.
  (defun gtags-update ()
    "Update gtags data."
    (interactive)
    (start-process "gtags-update" nil "global" "-u"))
  ; (add-hook 'after-save-hook 'gtags-update) ;ahh, no

  ;; visit current file under cursor.
  (defun my-gtags-find-file ()
    "Gtags find file, and jump to last exit position."
    (interactive)
    (gtags-find-file)
    (pop-global-mark))

  ;; find current header file under cursor.
  (defun my-gtags-find-this-file ()
    "Gtags find current header file under cursor."
    (interactive)
    (let (tagname)
      (setq tagname (concat (current-word) ".h"))
      (gtags-push-context)
      (gtags-goto-tag tagname "Po"))
    (pop-global-mark))
  (define-key gtags-mode-map [M-mouse-2] 'my-gtags-find-this-file)
)

(eval-after-load "gtags"
  '(my-gtags-settings))

(provide 'cm-global)
