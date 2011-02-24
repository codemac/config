;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; some stats
(require 'command-frequency)

(setq-default command-frequency-table-file "~/.emacs-frequency")

(command-frequency-table-load)
(command-frequency-mode 1)
(command-frequency-autosave-mode 1)

(provide 'cm-command-frequency)
