;; Add codemac/scm guix packages to guix .
(cons*
 (channel (name 'codemac-scm)
	  (url "https://github.com/codemac/scm.git"))
 (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (introduction
         (make-channel-introduction
          "33f86a4b48205c0dc19d7c036c85393f0766f806"
          (openpgp-fingerprint
           "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
 %default-channels)
