;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; jabber
;(require 'jabber)
(autoload 'jabber-connect-all "jabber" "" t)
;; Show my status in the header along with theirs! woo!
(eval-after-load 'jabber
  (progn
    (setq jabber-chat-header-line-format
          '(" " (:eval (jabber-jid-displayname jabber-chatting-with))
            " " (:eval (jabber-jid-resource jabber-chatting-with)) "\t";
            (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with)))
                     (propertize
                      (or
                       (cdr (assoc (get buddy 'show) jabber-presence-strings))
                       (get buddy 'show))
                      'face
                      (or (cdr (assoc (get buddy 'show) jabber-presence-faces))
                          'jabber-roster-user-online))))
            "\t" (:eval (get (jabber-jid-symbol jabber-chatting-with) 'status))
            (:eval (unless (equal "" *jabber-current-show*)
                     (concat "\t You're " *jabber-current-show*
                             " (" *jabber-current-status* ")")))))
    ;; Open urls!
    (add-hook 'jabber-chat-mode-hook 'goto-address)
    
    ;; fun keybindings!
    (defun my-jabber-chat-delete-or-bury ()
      (interactive)
      (if (eq 'jabber-chat-mode major-mode)
          (condition-case e 
              (delete-frame)
            (error 
             (if (string= "Attempt to delete the sole visible or iconified frame" 
                          (cadr e))
                 (bury-buffer))))))
    
;    (define-key jabber-chat-mode-map [escape] 'my-jabber-chat-delete-or-bury)
    (define-key mode-specific-map "jr"
      (lambda () 
        (interactive) 
        (switch-to-buffer "*-jabber-*")))
    (define-key mode-specific-map "jc"
      '(lambda () 
         (interactive) 
         (call-interactively 'jabber-connect)))
    (define-key mode-specific-map "jd"
      '(lambda () 
         (interactive) 
         (call-interactively 'jabber-disconnect)))
    (define-key mode-specific-map "jj"
      '(lambda () 
         (interactive) 
         (call-interactively 'jabber-chat-with)))
    (define-key mode-specific-map "ja"
      '(lambda () 
         (interactive) 
         (jabber-send-presence "away" "" 10)))
    (define-key mode-specific-map "jo"
      '(lambda () 
         (interactive) 
         (jabber-send-presence "" "" 10)))
    (define-key mode-specific-map "jx"
      '(lambda () 
         (interactive) 
         (jabber-send-presence "xa" "" 10)))))
;;;

(provide 'cm-jabber)
