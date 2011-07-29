;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; desktop

(require 'desktop)

(desktop-save-mode 1)

;; auto-save emacs instance
(defun cm-desktop-save ()
  (interactive)
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))

(add-hook 'auto-save-hook 'cm-desktop-save)

(provide 'cm-desktop)