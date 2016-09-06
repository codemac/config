;; init.scm -- default shepherd configuration file.

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(define (make-system-constructor command)
  (lambda _
    (run-command command)))
 
(define (make-system-destructor command)
  (lambda _
    (not (run-command command))))

(make <service>
  #:provides '(x)
  #:requires '()
  #:start (make-forkexec-constructor
	   '("Xorg" ":0" "-nolisten" "tcp" "-noreset" "-verbose" "2" "vt1"))
  #:stop (make-kill-destructor))

(make <service>
  #:provides '(emacs)
  #:requires '()
  #:start (make-system-constructor '("emacs" "--daemon"))
  #:stop (make-system-destructor '("emacsclient" "--eval" "(kill-emacs)")))

(make <service>
  #:provides '(sxhkd)
  #:requires '(x)
  #:start (make-forkexec-constructor
	   '("sxhkd"))
  #:stop (make-kill-destructor))

(make <service>
  #:provides '(syndaemon)
  #:requires '(x)
  #:start (make-system-constructor '("syndaemon" "-i" "1.0" "-R"))
  #:stop (make-kill-destructor))

(make <service>
  #:provides '(bitlbee)
  #:requires '()
  #:start (make-forkexec-constructor
	   '("bitlbee" "-F" "-n" "-d" "/home/codemac/.config/bitlbee"))
  #:stop (make-kill-destructor))

(define (cbattsvc batnum)
  (make <service>
    #:provides (list (string->symbol (string-append "cbatticon_" batnum)))
    #:requires '(x)
    #:start (make-forkexec-constructor
	     `("cbatticon" "-i" "standard" "-u" "30" "-l" "15" "-r" "2" ,batnum))
    #:stop (make-kill-destructor)))

;; TODO find every battery in /sys and auto generate this list! Also
;; consider making it take an "ordering" so we can determine what
;; order that shepherd launches them in.
(cbattsvc "BAT0")
(cbattsvc "BAT1")

;; gpg doesn't have a 'foreground' mode. So what I do instead is a
;; silly hack, I run gpg-agent and then check what the pid is that is
;; listening on /run/user/1000/S.gpg-agent. Due to my /mnt/keys
;; shenanigans, this is a surefire way of finding gpg-agent. If
;; gpg-agent dies, I restart it.

(define (gpgagent-start)
  (let ((gpgpid (fork+exec-command '("/usr/bin/gpg-agent" "--homedir=/mnt/keys/gnupghome"))))
    ; find /run/user/1000/S.gpg-agent
    ))

;; creates a cgroup at some root (in our case /sys/fs/cgroup) with a
;; set of attributes defined (can be none) This makes 
(define (make-system-cgroup-constructor))

(define (make-kill-cgroup-destructor))

(make <service>
  #:provides '(gpgagent)
  #:requires '()
  #:start (make-system-constructor
	   '("gpg-agent" "--homedir=/mnt/keys/gnupghome"))
  #:stop (make-kill-destructor))

;; Send shepherd into the background
(action 'shepherd 'daemonize)

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start
	  '(xterm
	    gpgagent
	    cbatticon_BAT0
	    cbatticon_BAT1
	    bitlbee
	    sxhkd
	    emacs
	    x
	    ))
