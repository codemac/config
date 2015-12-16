Ensure that go-autocomplete in your load-path and add to your ~/.emacs
following line:

(require 'go-autocomplete)

Also you could setup any combination (for example M-TAB)
for invoking auto-complete:

(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
