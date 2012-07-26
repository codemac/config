;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; erc
;;
(autoload 'erc "erc" "" t)
;; some stuff stolen from pmade
;; <http://pmade.com/svn/oss/rc/trunk/emacs/emacs.d/pmade/erc.el>
;; Basic IRC Settings
(setq erc-user-full-name "codemac")
(setq erc-email-userid "j@codemac.net")
(setq erc-nick "codemac")

;; ERC Time stamps
(setq erc-timestamp-only-if-changed-flag nil)
(setq erc-timestamp-format "[%H:%M:%S] ")
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)

;; Auto-fill (static size so log files look decent)
(setq erc-fill-column 78)
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 15)

;; Ignore messages from the server that are not channel activity
;(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
;                                "324" "329" "332" "333" "353" "477"))
;(setq erc-track-exclude '("&bitlbee" "#emacs" "#ruby" "#applescript"))

;; Auto join the given channels
(setq erc-autojoin-channels-alist cm-irc-channel-alist)

;; Some other settings
(setq erc-prompt 'my-erc-prompt)
(setq erc-max-buffer-size 20000)
(setq erc-track-showcount t)
(setq erc-auto-query 'bury)             ; Private messages go to a hidden buffer
(setq erc-query-display 'buffer)        ; Reuse current buffer when sending private messages
(setq erc-keywords '("codemac" "jeff"))

;; Setup ERC buffers
(defun my-erc-hook ()
  "Correctly configure ERC buffers"
  (auto-fill-mode 0)                    ; disable auto fill
  (setq truncate-lines nil)            ; wrap lines
  ;; Add some modules
  (add-to-list 'erc-modules 'spelling)
  (add-to-list 'erc-modules 'scrolltobottom)
  (add-to-list 'erc-modules 'truncate)
;(add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'highlight-nicknames)
  (erc-update-modules))

(defun my-erc-after-connect (server nick)
  (cond
   ((string-match "localhost" server) (erc-message "PRIVMSG" (concat "&bitlbee identify " cm-bitlbee-pass)))
   ((string-match "freenode"  server) (erc-message "PRIVMSG" (concat "NickServ identify " cm-freenode-pass)))
   ((string-match "oftc"      server) (erc-message "PRIVMSG" (concat "nickserv identify " cm-oftc-pass)))
   ((string-match "what"      server) (erc-message "PRIVMSG" (concat "Drone enter #what.cd codemac " cm-what-pass)))
))


;; Better Prompt
(defun my-erc-prompt ()
  (if (and (boundp 'erc-default-recipients) (erc-default-target))
      (erc-propertize (concat "[ " (erc-default-target) " ]") 'read-only t 'rear-nonsticky t 'front-nonsticky t)
    (erc-propertize (concat "[ ERC ]") 'read-only t 'rear-nonsticky t 'front-nonsticky t)))

;; Load in some ERC extra modules (you must download these separately)
(autoload 'erc-highlight-nicknames "erc-hightlight-nicknames")
;(require 'erc-highlight-nicknames)
(autoload 'erc-nicklist "erc-nicklist")
;(require 'erc-nicklist)
(setq erc-nicklist-use-icons nil)
(setq erc-nicklist-voiced-position 'top)



;; Hook in
(add-hook 'erc-mode-hook 'my-erc-hook)
(add-hook 'erc-after-connect 'my-erc-after-connect)

;; Start a local bitlbee server
;(require 'bitlbee)
;(setq bitlbee-user-directory "~/.bitlbee")
;(setq bitlbee-executable "/usr/sbin/bitlbee")
;(bitlbee-start)

;; Give bitlbee a chance to bind to the local port
;(sleep-for 1)

;; Define my ultracool erc-startup
(defun erc-startup ()
  (interactive)
  (erc-ssl :server "irc.freenode.net" :port "6697")
;  (erc :server "localhost" :port "6667")
;  (erc-ssl :server "irc.oftc.net" :port "6697")
)
;;;

(provide 'cm-erc)
