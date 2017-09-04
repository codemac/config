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


;;; Library code for everything!

;; handle ~/ essentially
(define (tilde . rest)
  (apply string-append (getenv "HOME") rest))

;; Execute commands directly
(define (make-exec-constructor cmd)
  (lambda args
    (zero? (status:exit-val (apply system* cmd)))))

;; Execute destruction commands directly
(define (make-exec-destructor cmd)
  (lambda (ignored . args)
    (zero? (status:exit-val (apply system* cmd)))))

;; Initialize a basic ssh-agent. I need to switch this to gpg-agent at
;; some point, but that's scary.
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

;; Run a shell command with sh, useful for sh parsing of things
(define (shellrunner a)
  (zero? (status:exit-val (apply system* (list "sh" "-c" a)))))


;;; Service Definitions.

(define (svc-bitlbee)
  (make <service>
    #:provides '(bitlbee)
    #:requires '()
    #:environment (environ)
    #:start (make-forkexec-constructor
	     `("bitlbee" "-F" "-n" "-d" ,(tilde "/.config/bitlbee")))
    #:stop (make-kill-destructor)))

(define (svc-blueman)
  (make <service>
    #:provides '(blueman)
    #:requires '(tray)
    #:environment (environ)
    #:start (make-forkexec-constructor '("blueman-applet"))
    #:stop (make-kill-destructor)))

(define (svc-cbatticon batnum)
  (make <service>
    #:provides (list (string->symbol (string-append "cbatticon_" batnum)))
    #:requires '(x)
    #:environment (environ)
    #:start (make-forkexec-constructor
	     `("cbatticon" "-i" "standard" "-u" "30" "-l" "15" "-r" "2" ,batnum))
    #:stop (make-kill-destructor)))

(define (svc-emacs)
  (make <service>
    #:provides '(emacs)
    #:requires '()
    #:environment (environ)
    #:start (make-forkexec-constructor '("emacs" "--daemon"))
    #:stop (make-exec-destructor '("emacsclient" "--eval" "(kill-emacs)"))))

(define (svc-mcron)
  #:provides '(mcron)
  #:requires '()
  #:environment (environ)
  #:start (make-exec-constructor '("mcron"))
  #:stop (make-kill-destructor))

(define (svc-networkicon)
  (make <service>
    #:provides '(networkicon)
    #:requires '(tray)
    #:environment (environ)
    #:start (make-forkexec-constructor '("nm-applet"))
    #:stop (make-kill-destructor)))

(define (svc-notion)
  (make <service>
    #:provides '(notion wm)
    #:requires '(x)
    #:environment (environ)
    #:start (make-forkexec-constructor '("notion"))
    #:stop (make-kill-destructor)))

(define (svc-pulseaudio)
  (make <service>
    #:provides '(audio pulseaudio)
    #:requires '()
    #:environment (environ)
    #:start (make-exec-constructor '("pulseaudio" "--start"))
    #:stop (make-kill-destructor)))

(define (svc-redshift)
  (make <service>
    #:provides '(redshift)
    #:requires '(tray)
    #:environment (environ)
    #:start (make-forkexec-constructor '("redshift-gtk"))
    #:stop (make-kill-destructor)))

(define (svc-rescuetime)
  (make <service>
    #:provides '(rescuetime)
    #:requires '(tray)
    #:environment (environ)
    #:start (make-forkexec-constructor '("rescuetime"))
    #:stop (make-kill-destructor)))

(define (svc-syndaemon)
  (make <service>
    #:provides '(syndaemon)
    #:requires '(x)
    #:environment (environ)
    #:start (make-exec-constructor '("syndaemon" "-i" "1.0" "-R" "-d"))
    #:stop (make-kill-destructor)))

(define (svc-trayion)
  (make <service>
    #:provides '(tray trayion)
    #:requires '(notion)
    #:environment (environ)
    #:start (make-forkexec-constructor
	     '("trayion"))
    #:stop (make-kill-destructor)))

(define (svc-volumeicon)
  (make <service>
    #:provides '(volumeicon)
    #:requires '(tray)
    #:environment (environ)
    #:start (make-forkexec-constructor
	     '("volumeicon"))
    #:stop (make-kill-destructor)))

(define (svc-x)
  (make <service>
    #:provides '(x)
    #:requires '()
    #:environment (environ)
    #:start (make-forkexec-constructor
	     '("Xorg" ":0" "-nolisten" "tcp" "-noreset" "-verbose" "2" "vt1"))
    #:stop (make-kill-destructor)))

(define (svc-xscreensaver)
  (make <service>
    #:provides '(screensaver xscreensaver)
    #:requires '(x)
    #:environment (environ)
    #:start (make-forkexec-constructor
	     '("xscreensaver" "-no-splash"))
    #:stop (make-kill-destructor)))


;;; Machine definitions.
(define-class <machine> ()
  (register-svcs #:init-keyword #:register-svcs
		 #:init-value '())
  (user-prehook #:init-keyword #:user-prehook
		#:init-value noop)
  (user-svcs #:init-keywoard #:user-svcs
	     #:init-value '())
  (graphical-prehook #:init-keyword #:graphical-prehook
		     #:init-value noop)
  (graphical-svcs #:init-keyword #:graphical-svcs
		  #:init-value '()))

(define glaptop-machine
  (make <machine>
    #:host-detector "jmickey-glaptop0"
    #:register-svcs (list
		     (svc-mcron))
    #:user-prehook (lambda ()
		     (setenv "GUILE_LOAD_PATH" (tilde "/scm")))
    #:user-svcs '(mcron)))

(define neah-machine
  (make <machine>
    #:host-detector "neah"
    #:register-svcs
    (list
     (svc-x)
     (svc-trayion)
     (svc-emacs)
     (svc-volumeicon)
     (svc-pulseaudio)
     (svc-syndaemon)
     (svc-networkicon)
     (svc-bitlbee)
     (svc-xscreensaver)
     (svc-blueman)
     (svc-rescuetime)
     (svc-mcron)
     (cbattsvc "BAT0")
     (cbattsvc "BAT1"))
    #:user-prehook
    (lambda ()
      ;; depressed I have to set this, but it's easier to sanitize the
      ;; environment.
      (setenv "DISPLAY" ":0.0")
      ;; q: why is perl in core_perl / vendor_perl?
      (setenv "PATH" (string-append
		      (tilde "/work/bin:")
		      (tilde "/bin")
		      ":/bin:/bin/core_perl:/bin/vendor_perl"))

      (setenv "LANGUAGE" "en_US.UTF-8")
      (setenv "MOZ_USE_OMTC" "1")
      (setenv "ABSROOT" (tilde "/src/abs"))
      (setenv "GOPATH" (tilde "/src/go"))
      (setenv "GUILE_LOAD_PATH" (tilde "/scm"))
      (setenv "_JAVA_OPTIONS" "-Dawt.useSystemAAFontSettings=on -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.aatext=true")
      (ssh-agent-initialize))
    #:user-svcs
    '(mcron)
    #:graphical-prehook
    (lambda ()
      (for-each
       shellrunner
       '("sleep 2"
	 "setxkbmap -device $(xinput list 'AT Translated Set 2 keyboard' | cut -d= -f2 | cut -f 1) -layout dvorak -option ctrl:swapcaps"
	 "xsetroot -solid '#80a0af'"
	 "xset r rate 200 20"
	 "xrandr --dpi 144")))))

(define machines (list glaptop-machine
		       neah-machine))


;;; Run the machine!
(define (current-machine)
  (define (check-machine m)
    (cond
     ((procedure? hn)
      (hn))
     ((string? hn)
      (equal? hn (gethostname)))
     (else
      (error "Machine incorrectly configured the host-detector, it must be a string or a procedure"))))

  (let loop ((mlist machines))
    (when (null? mlist)
	(error "Current machine not found!"))
    (let ((hn (slot-ref (car mlist) 'host-detector)))
      (if (check-machine hn)
	  (car mlist)
	  (loop (cdr mlist))))))

(define-method (run (m <machine>))
  (apply register-services (slot-ref m 'register-svcs))
  ((slot-ref m 'user-prehook))
  (for-each start (slot-ref m 'user-svcs))
  (unless (null? (slot-ref m 'graphical-svcs))
    (start 'x)
    ((slot-ref m 'graphical-prehook))
    (for-each start (slot-ref m 'graphical-svcs))))

;;(run (current-machine))

(format #t "Current machine: ~s~%" (current-machine))

