;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; w3m
;;
;(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m-load" "" t)
;(eval-after-load 'w3m-load
;  (progn
;    (require 'mime-w3m)
;    (setq w3m-use-cookies t)))

;; rockin' browse-url.
;; Browse OWA urls in w3m, browse others in firefox

; '(browse-url-browser-function 'cm-browse-url)

(provide 'cm-w3m)
