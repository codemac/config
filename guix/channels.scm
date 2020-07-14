;; Add codemac/scm guix packages to guix .
(cons
 (channel (name 'codemac-scm)
	  (url "https://github.com/codemac/scm.git"))
 %default-channels)
