;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elscreen
;; Elscreen - http://www.emacswiki.org/cgi-bin/wiki/EmacsLispScreen
;(require 'elscreen)
(autoload 'elscreen-one-screen-p "elscreen" "" t) 
(autoload 'elscreen-toggle "elscreen" "" t) 
(autoload 'elscreen-create "elscreen" "" t) 
(global-set-key (kbd "<C-tab>") 'elscreen-toggle)
(global-set-key [(control shift right)] 'elscreen-next)
(global-set-key [(control shift left)] 'elscreen-previous)
(global-set-key [(control t)] 'elscreen-create)

;;; C-x C-c closes frame or tab
(global-set-key "\C-x\C-c" 'intelligent-kill)

(defun intelligent-kill ()
  "quit the same way no matter what kind of window you are on"
  (interactive)
;  (kill-buffer (buffer-name))
  (if (and (not (elscreen-one-screen-p)) (elscreen-kill))
      (message "Killed screen")
    (if (eq (car (visible-frame-list)) (selected-frame))
        ;;for parent/master frame…
        (if (> (length (visible-frame-list)) 1)
            ;;a parent with children present
            (delete-frame (selected-frame))
          ;;a parent with no children present
          (save-buffers-kill-emacs))
      ;;a child frame
      (delete-frame (selected-frame)))))
;;;

(provide 'cm-elscreen)
