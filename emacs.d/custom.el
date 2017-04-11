(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "4e72cb2841e4801ba202a120c1cffdf88f5512536e557d03b3626d890b52f201" "36a309985a0f9ed1a0c3a69625802f87dee940767c9e200b89cdebdb737e5b29" "bf7ed640479049f1d74319ed004a9821072c1d9331bc1147e01d22748c18ebdf" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(haskell-hoogle-command "hoogle")
 '(ledger-reports
   (quote
    (("equity" "ledger -f journal.ledger equity")
     ("bal" "ledger -f %(ledger-file) bal")
     ("reg" "ledger -f %(ledger-file) reg")
     ("payee" "ledger -f %(ledger-file) reg @%(payee)")
     ("account" "ledger -f %(ledger-file) reg %(account)"))))
 '(notmuch-crypto-process-mime t)
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-inbox notmuch-hello-insert-alltags notmuch-hello-insert-recent-searches notmuch-hello-insert-footer)))
 '(package-selected-packages
   (quote
    (org-projectile zenburn-theme xelb xcscope wgrep vlf use-package tup-mode spinner solarized-theme smtpmail-multi slime simple-mpc rust-mode ruby-electric queue puml-mode protobuf-mode pretty-mode perspective pcmpl-git paredit pandoc-mode org-plus-contrib nyan-mode nrepl muse multiple-cursors mode-compile minimap mingus markdown-mode mark-multiple magit lua-mode load-theme-buffer-local js2-mode jabber inf-ruby impatient-mode identica-mode iasm-mode helm-projectile-all helm-projectile helm-gtags helm-go-package helm-c-moccur haskell-mode go-eldoc go-autocomplete geiser flycheck-tip fill-column-indicator f expand-region evil ess-smart-underscore ess-R-object-popup ess-R-data-view erc-image erc-hl-nicks epresent emms emagician-fix-spell-memory elfeed-org ein diffscuss-mode diff-hl d-mode command-frequency centered-window-mode buffer-move browse-kill-ring bbdb auctex ascope anything android-mode ace-jump-mode)))
 '(protect-buffer-bury-p nil t)
 '(safe-local-variable-values
   (quote
    ((tab-width 8)
     (eval when
	   (require
	    (quote rainbow-mode)
	    nil t)
	   (rainbow-mode 1))
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (bug-reference-bug-regexp . "<https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/\\([0-9]+\\)>")
     (yaml-indent-offset . 8)
     (after-save-hook archive-done-tasks))))
 '(smtpmail-queue-mail nil t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(yaml-indent-offset 8))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
