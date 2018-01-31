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
(define* (make-exec-constructor cmd #:key (environment-variables #f))
  (lambda (arg)
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
  (and-let* ((p (open-pipe* OPEN_READ "ssh-agent"))
	     (authsock (read-line p))
	     (authpid (read-line p))
	     (echoline (read-line p))
	     (eofp (eof-object? (read-line p)))
	     (sshauthsock (envval authsock))
	     (sshagentpid (envval authpid)))
    ;; note we do not call close-pipe, the process is gone anyways and guile signals an error any time you wait on an already exited child (whyyy)
    (setenv "SSH_AUTH_SOCK" sshauthsock)
    (setenv "SSH_AGENT_PID" sshagentpid)
    (zero? (status:exit-val (system* "ssh-add")))))

;; Run a shell command with sh, useful for sh parsing of things
(define (shellrunner a)
  (zero? (status:exit-val (apply system* (list "sh" "-c" a)))))

(define *the-environment* (environ))


;;; Service Definitions.

(define (svc-bitlbee)
  (make <service>
    #:provides '(bitlbee)
    #:requires '()
    #:start (make-forkexec-constructor
	     `("bitlbee" "-F" "-n" "-d" ,(tilde "/.config/bitlbee"))
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-blueman)
  (make <service>
    #:provides '(blueman)
    #:requires '(tray)
    #:start (make-forkexec-constructor '("blueman-applet")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-cbatticon batnum)
  (make <service>
    #:provides (list (string->symbol (string-append "cbatticon_" batnum)))
    #:requires '(tray)
    #:start (make-forkexec-constructor
	     `("cbatticon" "-i" "standard" "-u" "30" "-l" "15" "-r" "2" ,batnum)
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-emacs)
  (make <service>
    #:provides '(emacs)
    #:requires '(x)
    #:start (make-forkexec-constructor '("emacs" "--daemon")
	     #:environment-variables *the-environment*)
    #:stop (make-exec-destructor '("emacsclient" "--eval" "(kill-emacs)"))))

(define (svc-mcron)
  #:provides '(mcron)
  #:requires '()
  #:start (make-exec-constructor '("mcron")
	     #:environment-variables *the-environment*)
  #:stop (make-kill-destructor))

(define (svc-networkicon)
  (make <service>
    #:provides '(networkicon)
    #:requires '(tray)
    #:start (make-forkexec-constructor '("nm-applet")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-notion)
  (make <service>
    #:provides '(notion wm)
    #:requires '(x)
    #:start (make-forkexec-constructor '("notion")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-pulseaudio)
  (make <service>
    #:provides '(audio pulseaudio)
    #:requires '()
    #:start (make-exec-constructor '("pulseaudio" "--start")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-redshift)
  (make <service>
    #:provides '(redshift)
    #:requires '(tray)
    #:start (make-forkexec-constructor '("redshift-gtk")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-rescuetime)
  (make <service>
    #:provides '(rescuetime)
    #:requires '(tray)
    #:start (make-forkexec-constructor '("rescuetime")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-syndaemon)
  (make <service>
    #:provides '(syndaemon)
    #:requires '(x)
    #:start (make-exec-constructor '("syndaemon" "-i" "1.0" "-R" "-d")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-trayion)
  (make <service>
    #:provides '(tray trayion)
    #:requires '(notion)
    #:start (make-forkexec-constructor
	     '("trayion")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-volumeicon)
  (make <service>
    #:provides '(volumeicon)
    #:requires '(tray)
    #:start (make-forkexec-constructor
	     '("volumeicon")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-x)
  (make <service>
    #:provides '(x)
    #:requires '()
    #:start (make-forkexec-constructor
	     `("Xorg" ":0.0" "-nolisten" "tcp" "-noreset" "-verbose" "2" ,(string-append "vt" (getenv "XDG_VTNR")))
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))

(define (svc-xscreensaver)
  (make <service>
    #:provides '(screensaver xscreensaver)
    #:requires '(x)
    #:start (make-forkexec-constructor
	     '("xscreensaver" "-no-splash")
	     #:environment-variables *the-environment*)
    #:stop (make-kill-destructor)))


;;; Machine definitions.
(define-class <machine> ()
  (host-detector #:init-keyword #:host-detector
		 #:init-value "")
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
     (svc-notion)
     (svc-trayion)
     (svc-emacs)
     (svc-volumeicon)
     (svc-pulseaudio)
     (svc-networkicon)
     (svc-bitlbee)
     (svc-xscreensaver)
     (svc-blueman)
     (svc-rescuetime)
     (svc-redshift)
     (svc-cbatticon "BAT0")
     (svc-cbatticon "BAT1"))
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
    #:graphical-prehook
    (lambda ()
      (for-each
       shellrunner
       `("sleep 2"
	 "setxkbmap -device $(xinput list 'AT Translated Set 2 keyboard' | cut -d= -f2 | cut -f 1) -layout dvorak"
	 ,(string-append "xmodmap " (tilde "/.Xmodmap"))
	 "xsetroot -solid '#80a0af'"
	 "xset r rate 200 20"
	 "xrandr --dpi 96")))
    #:graphical-svcs
    '(notion trayion
       volumeicon networkicon xscreensaver blueman cbatticon_BAT0 redshift)))

(define machines (list glaptop-machine
		       neah-machine))


;;; Run the machine!
(define (current-machine)
  (define (check-machine hn)
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
  ((slot-ref m 'user-prehook))
  (set! *the-environment* (environ))
  (apply register-services (slot-ref m 'register-svcs))
  (for-each start (slot-ref m 'user-svcs))
  (unless (null? (slot-ref m 'graphical-svcs))
    (start 'x)
    (environ *the-environment*)
    (format #t "*the-environment*: ~s~%" *the-environment*)
    ((slot-ref m 'graphical-prehook))
    (for-each start (slot-ref m 'graphical-svcs))))

(run (current-machine))

