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

(use-modules (srfi srfi-2)
	     (ice-9 popen)
	     (ice-9 rdelim))

(define (tilde . rest)
  (apply string-append (getenv "HOME") rest))

;; I hate /bin/sh
(define (make-exec-constructor cmd)
  (lambda args
    (zero? (status:exit-val (apply system* cmd)))))

(define (make-exec-destructor cmd)
  (lambda (ignored . args)
    (zero? (status:exit-val (apply system* cmd)))))

(define (cbattsvc batnum)
  (make <service>
    #:provides (list (string->symbol (string-append "cbatticon_" batnum)))
    #:requires '(x)
    #:environment *global-environment*
    #:start (make-forkexec-constructor
	     `("cbatticon" "-i" "standard" "-u" "30" "-l" "15" "-r" "2" ,batnum))
    #:stop (make-kill-destructor)))

;; set up my ssh-agent

(define (ssh-agent-initialize)
  (define (envval s)
    (cadr (string-split (car (string-split s #\;)) #\=)))
  (and-let* ((p (open-pipe* OPEN_BOTH "ssh-agent"))
	     (authsock (read-line p))
	     (authpid (read-line p))
	     (echoline (read-line p))
	     (eofp (eof-object? (read-line p)))
	     (sshauthsock (envval authsock))
	     (sshagentpid (envval authpid)))
    (setenv "SSH_AUTH_SOCK" sshauthsock)
    (setenv "SSH_AGENT_PID" sshagentpid)
    (zero? (status:exit-val (system* "ssh-add")))))

(ssh-agent-initialize)

;; depressed I have to set this, but it's easier to sanitize the
;; environment.
(setenv "DISPLAY" ":0.0")
;; q: why is perl in core_perl / vendor_perl?
(setenv "PATH" (string-append
		 (tilde "/work/bin:")
		 (tilde "/bin:")
		 "/bin:/bin/core_perl:/bin/vendor_perl"))
(setenv "LANGUAGE" "en_US.UTF-8")
(setenv "MOZ_USE_OMTC" "1")
(setenv "ABSROOT" (tilde "/src/abs"))
(setenv "GOPATH" (tilde "/src/go"))
(setenv "GUILE_LOAD_PATH" (tilde "/scm"))
(setenv "_JAVA_OPTIONS" "-Dawt.useSystemAAFontSettings=on -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.aatext=true")

(define *global-environment* (environ))

(define (shellrunner a)
  (zero? (status:exit-val (apply system* (list "sh" "-c" a)))))

(define (host-dpi host)
  (cond
    ((equal? host "neah")
     96)
    ((equal? host "nevada")
     144)
    (else
      96)))

(define (initx)
  (for-each
    shellrunner
    `("sleep 2"
      "setxkbmap -device $(xinput list 'AT Translated Set 2 keyboard' | cut -d= -f2 | cut -f 1) -layout dvorak"
      ,(string-append "xmodmap " (tilde "/.Xmodmap"))
      "xsetroot -solid '#80a0af'"
      "xset r rate 200 20"
      ,(string-append "xrandr --dpi " (number->string (host-dpi (gethostname)))))))

(register-services
 (make <service>
   #:provides '(x)
   #:requires '()
   #:environment *global-environment*
   #:start (make-forkexec-constructor
	    '("Xorg" ":0" "-nolisten" "tcp" "-noreset" "-verbose" "2" "vt1"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(tray trayion)
   #:requires '(notion)
   #:environment *global-environment*
   #:start (make-forkexec-constructor
	    '("trayion"))
   #:stop (make-kill-destructor))
 
 (make <service>
   #:provides '(emacs)
   #:requires '()
   #:environment *global-environment*
   #:start (make-forkexec-constructor '("emacs" "--daemon"))
   #:stop (make-exec-destructor '("emacsclient" "--eval" "(kill-emacs)")))

 (make <service>
   #:provides '(volumeicon)
   #:requires '(tray)
   #:environment *global-environment*
   #:start (make-forkexec-constructor
	    '("volumeicon"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(audio pulseaudio)
   #:requires '()
   #:environment *global-environment*
   #:start (make-exec-constructor '("pulseaudio" "--start"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(syndaemon)
   #:requires '(x)
   #:environment *global-environment*
   #:start (make-exec-constructor '("syndaemon" "-i" "1.0" "-R" "-d"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(networkicon)
   #:requires '(tray)
   #:environment *global-environment*
   #:start (make-forkexec-constructor '("nm-applet"))
   #:stop (make-kill-destructor))
 
 (make <service>
   #:provides '(bitlbee)
   #:requires '()
   #:environment *global-environment*
   #:start (make-forkexec-constructor
	    `("bitlbee" "-F" "-n" "-d" ,(tilde "/.config/bitlbee")))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(screensaver xscreensaver)
   #:requires '(x)
   #:environment *global-environment*
   #:start (make-forkexec-constructor
	    '("xscreensaver" "-no-splash"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(notion wm)
   #:requires '(x)
   #:environment *global-environment*
   #:start (make-forkexec-constructor '("notion"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(redshift)
   #:requires '(tray)
   #:environment *global-environment*
   #:start (make-forkexec-constructor '("redshift-gtk"))
   #:stop (make-kill-destructor))

 (make <service>
   #:provides '(blueman)
   #:requires '(tray)
   #:environment *global-environment*
   #:start (make-forkexec-constructor '("blueman-applet"))
   #:stop (make-kill-destructor))
 
 (make <service>
   #:provides '(rescuetime)
   #:requires '(tray)
   #:environment *global-environment*
   #:start (make-forkexec-constructor '("rescuetime"))
   #:stop (make-kill-destructor))
 
 (cbattsvc "BAT0")
 (cbattsvc "BAT1"))

(start 'x)

(initx)

(for-each start
	  '(notion
	    pulseaudio
	    cbatticon_BAT0
	    cbatticon_BAT1
	    screensaver
	    networkicon
	    volumeicon
	    rescuetime
	    blueman
	    redshift))
