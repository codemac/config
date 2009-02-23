;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ruby-mode
;;
(autoload 'ruby-mode "ruby-mode"
    "Mode for editing ruby source files")
(autoload 'run-ruby "inf-ruby"
    "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
     '(lambda ()
         (inf-ruby-keys)))
 ;; If you have Emacs 19.2x or older, use rubydb2x                              
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)
 ;; uncomment the next line if you want syntax highlighting                     
(add-hook 'ruby-mode-hook 'turn-on-font-lock)

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))

(provide 'cm-ruby)