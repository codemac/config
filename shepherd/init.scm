;; init.scm -- default shepherd configuration file.

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.

;; TODO find every battery in /sys and auto generate this list! Also
;; consider making it take an "ordering" so we can determine what
;; order that shepherd launches them in.

;; gpg doesn't have a 'foreground' mode. So what I do instead is a
;; silly hack, I run gpg-agent and then check what the pid is that is
;; listening on /run/user/1000/S.gpg-agent. Due to my /mnt/keys
;; shenanigans, this is a surefire way of finding gpg-agent. If
;; gpg-agent dies, I restart it.

;; Also, check out jailing for processes, notably with user
;; containers. Probably using something like minijail0

(define (tilde . rest)
  (apply string-append (getenv "HOME") rest))

;; this command forks away, and runs in the background. I hate /bin/sh
(define (make-exec-constructor . args)
  (lambda args
    (zero? (status:exit-val (apply system* args)))))

(define (make-exec-destructor . args)
  (lambda args
    (zero? (status:exit-val (apply system* args)))))

(define (cbattsvc batnum)
  (make <service>
    #:provides (list (string->symbol (string-append "cbatticon_" batnum)))
    #:requires '(x)
    #:start (make-forkexec-constructor
	     `("cbatticon" "-i" "standard" "-u" "30" "-l" "15" "-r" "2" ,batnum))
    #:stop (make-kill-destructor)))

(register-services
 (make <service>
   #:provides '(x11 xorg x)
   #:requires '()
   #:start (make-forkexec-constructor
	    '("Xorg" ":0" "-nolisten" "tcp" "-noreset" "-verbose" "2" "vt1"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(tray trayion)
   #:requires '(x)
   #:start (make-forkexec-constructor
	    '("trayion"))
   #:stop (make-kill-destructor))
 
 (make <service>
   #:provides '(emacs)
   #:requires '()
   #:start (make-exec-constructor '("emacs" "--daemon"))
   #:stop (make-exec-destructor '("emacsclient" "--eval" "(kill-emacs)")))

 (make <service>
   #:provides '(sxhkd)
   #:requires '(x)
   #:start (make-forkexec-constructor
	    '("sxhkd"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(volumeicon)
   #:requires '(tray)
   #:start (make-forkexec-constructor
	    '("volumeicon"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(audio pulseaudio)
   #:requires '()
   #:start (make-exec-constructor '("pulseaudio" "--start"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(syndaemon)
   #:requires '(x)
   #:start (make-exec-constructor '("syndaemon" "-i" "1.0" "-R" "-d"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(networkicon)
   #:requires '(tray)
   #:start (make-forkexec-constructor '("nm-applet"))
   #:stop (make-kill-destructor))
 
 (make <service>
   #:provides '(bitlbee)
   #:requires '()
   #:start (make-forkexec-constructor
	    `("bitlbee" "-F" "-n" "-d" ,(tilde "/.config/bitlbee")))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(screensaver xscreensaver)
   #:requires '(x)
   #:start (make-forkexec-constructor
	    '("xscreensaver" "-no-splash"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(notion wm)
   #:requires '(x)
   #:start (make-forkexec-constructor '("notion"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(redshift)
   #:requires '(x)
   #:start (make-forkexec-constructor '("redshift-gtk"))
   #:stop (make-kill-destructor))
 
 (cbattsvc "BAT0")
 (cbattsvc "BAT1")
 
 (make <service>
  #:provides '(gpgagent)
  #:requires '()
  #:start (make-exec-constructor
	   '("gpg-agent" "--homedir=/mnt/keys/gnupghome"))
  #:stop (make-kill-destructor)))

;; a set-environment command
;(define (set-environment))

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start
	  '(x
	    notion
	    trayion
	    cbatticon_BAT0
	    cbatticon_BAT1
	    screensaver
	    networkicon
	    volumeicon
	    redshift
	    bitlbee
	    sxhkd
	    emacs))
