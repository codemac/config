;; init.scm -- default dmd configuration file.

;; Services known to dmd:
;; Add new services (defined using 'make <service>') to dmd here by
;; providing them as arguments to 'register-services'.


(define x
  (make <service>
    #:provides '(x11 xorg x)
    #:requires '()
    #:start (make-forkexec-constructor
	     '("/usr/bin/Xorg :0 -nolisten tcp -noreset -verbose 2 \"vt1\""))
    #:stop (make-kill-destructor)))

(define sxhkd
  (make <service>
    #:provides '(sxhkd hotkeys)
    #:requires '(x)
    #:start (make-forkexec-constructor
	     '("/usr/bin/sxhkd"))
    #:stop (make-kill-destructor)))

(define xterm
  (make <service>
    #:provides '(xterm)
    #:requires '()
    #:start (make-forkexec-constructor
	     '("/usr/bin/xterm"))
    #:stop (make-kill-destructor)))

(define bitlbee
  (make <service>
    #:provides '(bitlbee)
    #:requires '()
    #:start (make-forkexec-constructor
	     '("/usr/bin/bitlbee" "-F" "-n" "-d" "/home/codemac/.config/bitlbee"))
    #:stop (make-kill-destructor)))

(define (cbattsvc batnum)
  (make <service>
    #:provides `(,(string->symbol (string-append "cbatticon_" batnum)))
    #:requires '()
    #:start (make-forkexec-constructor
	     `("/usr/bin/cbatticon" "-i" "standard" "-u" "30" "-l" "15" "-r" "2" ,batnum))
    #:stop (make-kill-destructor)))

;; TODO find every battery in /sys and auto generate this list! Also
;; consider making it take an "ordering" so we can determine what
;; order that dmd launches them in.
(define cbatt0 (cbattsvc "BAT0"))
(define cbatt1 (cbattsvc "BAT1"))

;; gpg doesn't have a 'foreground' mode. So what I do instead is a
;; silly hack, I run gpg-agent and then check what the pid is that is
;; listening on /run/user/1000/S.gpg-agent. Due to my /mnt/keys
;; shenanigans, this is a surefire way of finding gpg-agent. If
;; gpg-agent dies, I restart it.

(define (initialize-cgroup-tracking)
  (mkdir "/sys/fs/cgroup/user-dmd"))
(define (gpgagent-start)
  (let ((gpgpid (fork+exec-command '("/usr/bin/gpg-agent" "--homedir=/mnt/keys/gnupghome"))))
    ; find /run/user/1000/S.gpg-agent
    ))

;; creates a cgroup at some root (in our case /sys/fs/cgroup) with a
;; set of attributes defined (can be none) This makes 
(define (make-system-cgroup-constructor))

(define (make-kill-cgroup-destructor))
(define gpgagent
  (make <service>
    #:provides '(gpg gpg2 gpgagent)
    #:requires '()
    #:start (make-system-constructor
	     '("/usr/bin/gpg-agent" "--homedir=/mnt/keys/gnupghome"))
    #:stop (make-kill-destructor)))

(register-services x sxhkd xterm bitlbee gpgagent cbatt0 cbatt1)

(initialize-cgroup-tracking)
;; Send dmd into the background
(action 'dmd 'daemonize)

;; Services to start when dmd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(xterm))
