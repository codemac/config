;;; jabber-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fsm" "fsm.el" (0 0 0 0))
;;; Generated autoloads from fsm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fsm" '("fsm-" "define-")))

;;;***

;;;### (autoloads nil "jabber" "jabber.el" (0 0 0 0))
;;; Generated autoloads from jabber.el

(defvar jabber-account-list nil "\
List of Jabber accounts.
Each element of the list is a cons cell describing a Jabber account,
where the car is a JID and the CDR is an alist.

JID is a full Jabber ID string (e.g. foo@bar.tld). You can also
specify the resource (e.g. foo@bar.tld/emacs).
The following keys can be present in the alist:
:password is a string to authenticate ourself against the server.
It can be empty.
:network-server is a string identifying the address to connect to,
if it's different from the server part of the JID.
:port is the port to use (default depends on connection type).
:connection-type is a symbol. Valid symbols are `starttls',
`network' and `ssl'.

Only JID is mandatory.  The rest can be guessed at run-time.

Examples:

Two accounts without any special configuration:
\((\"foo@example.com\") (\"bar@example.net\"))

One disabled account with a non-standard port:
\((\"romeo@montague.net\" (:port . 5242) (:disabled . t)))

If you don't have SRV and STARTTLS capabilities in your Emacs,
configure a Google Talk account like this:
\((\"username@gmail.com\" 
  (:network-server . \"talk.google.com\")
  (:connection-type . ssl)))")

(custom-autoload 'jabber-account-list "jabber" t)

(defvar *jabber-current-status* nil "\
the users current presence status")

(defvar *jabber-current-show* nil "\
the users current presence show")

(defvar *jabber-current-priority* nil "\
the user's current priority")

(defconst jabber-presence-faces '(("" . jabber-roster-user-online) ("away" . jabber-roster-user-away) ("xa" . jabber-roster-user-xa) ("dnd" . jabber-roster-user-dnd) ("chat" . jabber-roster-user-chatty) ("error" . jabber-roster-user-error) (nil . jabber-roster-user-offline)) "\
Mapping from presence types to faces")

(autoload 'jabber-customize "jabber" "\
customize jabber options

\(fn)" t nil)

(autoload 'jabber-info "jabber" "\
open jabber.el manual

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber" '("jabber-" "*jabber-status-history*")))

;;;***

;;;### (autoloads nil "jabber-activity" "jabber-activity.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-activity.el

(defvar jabber-activity-mode t "\
Non-nil if Jabber-Activity mode is enabled.
See the `jabber-activity-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `jabber-activity-mode'.")

(custom-autoload 'jabber-activity-mode "jabber-activity" nil)

(autoload 'jabber-activity-mode "jabber-activity" "\
Toggle display of activity in hidden jabber buffers in the mode line.

With a numeric arg, enable this display if arg is positive.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-activity" '("jabber-activity-")))

;;;***

;;;### (autoloads nil "jabber-ahc" "jabber-ahc.el" (0 0 0 0))
;;; Generated autoloads from jabber-ahc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ahc" '("jabber-ahc-")))

;;;***

;;;### (autoloads nil "jabber-ahc-presence" "jabber-ahc-presence.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-ahc-presence.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ahc-presence" '("jabber-ahc-presence")))

;;;***

;;;### (autoloads nil "jabber-alert" "jabber-alert.el" (0 0 0 0))
;;; Generated autoloads from jabber-alert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-alert" '("jabber-" "define-" "beep" "echo")))

;;;***

;;;### (autoloads nil "jabber-autoaway" "jabber-autoaway.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-autoaway.el

(autoload 'jabber-autoaway-start "jabber-autoaway" "\
Start autoaway timer.
The IGNORED argument is there so you can put this function in
`jabber-post-connect-hooks'.

\(fn &optional IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-autoaway" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-avatar" "jabber-avatar.el" (0 0 0 0))
;;; Generated autoloads from jabber-avatar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-avatar" '("jabber-avatar-" "avatar")))

;;;***

;;;### (autoloads nil "jabber-awesome" "jabber-awesome.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-awesome.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-awesome" '("awesome" "jabber-awesome-")))

;;;***

;;;### (autoloads nil "jabber-bookmarks" "jabber-bookmarks.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-bookmarks.el

(autoload 'jabber-get-conference-data "jabber-bookmarks" "\
Get bookmark data for CONFERENCE-JID.
KEY may be nil or one of :name, :autojoin, :nick and :password.
If KEY is nil, a plist containing the above keys is returned.
CONT is called when the result is available, with JC and the
result as arguments.  If CONT is nil, return the requested data
immediately, and return nil if it is not in the cache.

\(fn JC CONFERENCE-JID CONT &optional KEY)" nil nil)

(autoload 'jabber-parse-conference-bookmark "jabber-bookmarks" "\
Convert a <conference/> tag into a plist.
The plist may contain the keys :jid, :name, :autojoin,
:nick and :password.

\(fn NODE)" nil nil)

(autoload 'jabber-get-bookmarks "jabber-bookmarks" "\
Retrieve bookmarks (if needed) and call CONT.
Arguments to CONT are JC and the bookmark list.  CONT will be
called as the result of a filter function or a timer.
If REFRESH is non-nil, always fetch bookmarks.

\(fn JC CONT &optional REFRESH)" nil nil)

(autoload 'jabber-get-bookmarks-from-cache "jabber-bookmarks" "\
Return cached bookmarks for JC.
If bookmarks have not yet been fetched by `jabber-get-bookmarks',
return nil.

\(fn JC)" nil nil)

(autoload 'jabber-edit-bookmarks "jabber-bookmarks" "\
Create a buffer for editing bookmarks interactively.

\(fn JC)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-bookmarks" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-browse" "jabber-browse.el" (0 0 0 0))
;;; Generated autoloads from jabber-browse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-browse" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-chat" "jabber-chat.el" (0 0 0 0))
;;; Generated autoloads from jabber-chat.el

(defvar jabber-chatting-with nil "\
JID of the person you are chatting with")

(autoload 'jabber-chat-get-buffer "jabber-chat" "\
Return the chat buffer for chatting with CHAT-WITH (bare or full JID).
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn CHAT-WITH)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-chat" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-chatbuffer" "jabber-chatbuffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-chatbuffer.el

(defvar jabber-buffer-connection nil "\
The connection used by this buffer.")

(make-variable-buffer-local 'jabber-buffer-connection)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-chatbuffer" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-chatstates" "jabber-chatstates.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-chatstates.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-chatstates" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-compose" "jabber-compose.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-compose.el

(autoload 'jabber-compose "jabber-compose" "\
Create a buffer for composing a Jabber message.

\(fn JC &optional RECIPIENT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-compose" '("jabber-compose-send")))

;;;***

;;;### (autoloads nil "jabber-conn" "jabber-conn.el" (0 0 0 0))
;;; Generated autoloads from jabber-conn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-conn" '("jabber-" "*jabber-")))

;;;***

;;;### (autoloads nil "jabber-console" "jabber-console.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-console.el

(autoload 'jabber-process-console "jabber-console" "\
Log XML-DATA i/o as XML in \"*-jabber-console-JID-*\" buffer

\(fn JC DIRECTION XML-DATA)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-console" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-core" "jabber-core.el" (0 0 0 0))
;;; Generated autoloads from jabber-core.el
 (autoload 'jabber-connect-all "jabber" "Connect to all configured Jabber accounts.\nSee `jabber-account-list'.\nIf no accounts are configured (or ARG supplied), call `jabber-connect' interactively." t)
 (autoload 'jabber-connect "jabber" "Connect to the Jabber server and start a Jabber XML stream.\nWith prefix argument, register a new account.\nWith double prefix argument, specify more connection details." t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-core" '("jabber-" "*jabber-")))

;;;***

;;;### (autoloads nil "jabber-disco" "jabber-disco.el" (0 0 0 0))
;;; Generated autoloads from jabber-disco.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-disco" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-events" "jabber-events.el" (0 0 0 0))
;;; Generated autoloads from jabber-events.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-events" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-export" "jabber-export.el" (0 0 0 0))
;;; Generated autoloads from jabber-export.el

(autoload 'jabber-export-roster "jabber-export" "\
Export roster for connection JC.

\(fn JC)" t nil)

(autoload 'jabber-import-roster "jabber-export" "\
Create buffer for roster import for connection JC from FILE.

\(fn JC FILE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-export" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-feature-neg" "jabber-feature-neg.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-feature-neg.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-feature-neg" '("jabber-fn-")))

;;;***

;;;### (autoloads nil "jabber-ft-client" "jabber-ft-client.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ft-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ft-client" '("jabber-ft-")))

;;;***

;;;### (autoloads nil "jabber-ft-common" "jabber-ft-common.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ft-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ft-common" '("jabber-ft-")))

;;;***

;;;### (autoloads nil "jabber-ft-server" "jabber-ft-server.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ft-server.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ft-server" '("jabber-ft-")))

;;;***

;;;### (autoloads nil "jabber-gmail" "jabber-gmail.el" (0 0 0 0))
;;; Generated autoloads from jabber-gmail.el

(autoload 'jabber-gmail-subscribe "jabber-gmail" "\
Subscribe to gmail notifications.
See http://code.google.com/apis/talk/jep_extensions/usersettings.html#4

\(fn JC)" t nil)

(autoload 'jabber-gmail-query "jabber-gmail" "\
Request mail information from the Google Talk server (a.k.a. one shot query).
See http://code.google.com/apis/talk/jep_extensions/gmail.html#requestmail

\(fn JC)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-gmail" '("jabber-gmail-")))

;;;***

;;;### (autoloads nil "jabber-history" "jabber-history.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-history.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-history" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-iq" "jabber-iq.el" (0 0 0 0))
;;; Generated autoloads from jabber-iq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-iq" '("jabber-" "*jabber-open-info-queries*")))

;;;***

;;;### (autoloads nil "jabber-keepalive" "jabber-keepalive.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-keepalive.el

(let ((loads (get 'jabber-keepalive 'custom-loads))) (if (member '"jabber-keepalive" loads) nil (put 'jabber-keepalive 'custom-loads (cons '"jabber-keepalive" loads))))

(autoload 'jabber-keepalive-start "jabber-keepalive" "\
Activate keepalive.
That is, regularly send a ping request to the server, and
disconnect if it doesn't answer.  See `jabber-keepalive-interval'
and `jabber-keepalive-timeout'.

The JC argument makes it possible to add this function to
`jabber-post-connect-hooks'; it is ignored.  Keepalive is activated
for all accounts regardless of the argument.

\(fn &optional JC)" t nil)

(autoload 'jabber-whitespace-ping-start "jabber-keepalive" "\
Start sending whitespace pings at regular intervals.
See `jabber-whitespace-ping-interval'.

The JC argument is ignored; whitespace pings are enabled for all
accounts.

\(fn &optional JC)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-keepalive" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-keymap" "jabber-keymap.el" (0 0 0 0))
;;; Generated autoloads from jabber-keymap.el

(defvar jabber-global-keymap (let ((map (make-sparse-keymap))) (define-key map "" 'jabber-connect-all) (define-key map "" 'jabber-disconnect) (define-key map "" 'jabber-switch-to-roster-buffer) (define-key map "\n" 'jabber-chat-with) (define-key map "\f" 'jabber-activity-switch-to) (define-key map "" 'jabber-send-away-presence) (define-key map "" 'jabber-send-default-presence) (define-key map "" 'jabber-send-xa-presence) (define-key map "" 'jabber-send-presence) map) "\
Global Jabber keymap (usually under C-x C-j)")

(define-key ctl-x-map "\n" jabber-global-keymap)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-keymap" '("jabber-common-keymap")))

;;;***

;;;### (autoloads nil "jabber-libnotify" "jabber-libnotify.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-libnotify.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-libnotify" '("libnotify" "jabber-libnotify-")))

;;;***

;;;### (autoloads nil "jabber-logon" "jabber-logon.el" (0 0 0 0))
;;; Generated autoloads from jabber-logon.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-logon" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-menu" "jabber-menu.el" (0 0 0 0))
;;; Generated autoloads from jabber-menu.el

(defvar jabber-menu (let ((map (make-sparse-keymap "jabber-menu"))) (define-key map [jabber-menu-connect] '("Connect" . jabber-connect-all)) (define-key map [jabber-menu-nextmsg] '("Next unread message" . jabber-activity-switch-to)) (define-key map [jabber-menu-disconnect] '("Disconnect" . jabber-disconnect)) (define-key map [jabber-menu-roster] '("Switch to roster" . jabber-switch-to-roster-buffer)) (define-key map [jabber-menu-customize] '("Customize" . jabber-customize)) (define-key map [jabber-menu-info] '("Help" . jabber-info)) (define-key map [jabber-menu-status] (cons "Set Status" (make-sparse-keymap "set-status"))) (define-key map [jabber-menu-status jabber-menu-status-chat] '("Chatty" lambda nil (interactive) (jabber-send-presence "chat" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*))) (define-key map [jabber-menu-status jabber-menu-status-dnd] '("Do not Disturb" lambda nil (interactive) (jabber-send-presence "dnd" (jabber-read-with-input-method "status message: " *jabber-current-status* '*jabber-status-history*) *jabber-current-priority*))) (define-key map [jabber-menu-status jabber-menu-status-xa] '("Extended Away" . jabber-send-xa-presence)) (define-key map [jabber-menu-status jabber-menu-status-away] '("Away" . jabber-send-away-presence)) (define-key map [jabber-menu-status jabber-menu-status-online] '("Online" . jabber-send-default-presence)) map))

(defvar jabber-display-menu 'maybe "\
Decide whether the \"Jabber\" menu is displayed in the menu bar.
If t, always display.
If nil, never display.
If maybe, display if any of `jabber-account-list' or `jabber-connections'
is non-nil.")

(custom-autoload 'jabber-display-menu "jabber-menu" t)

(define-key-after (lookup-key global-map [menu-bar]) [jabber-menu] (list 'menu-item "Jabber" jabber-menu :visible '(or (eq jabber-display-menu t) (and (eq jabber-display-menu 'maybe) (or jabber-account-list (bound-and-true-p jabber-connections))))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-menu" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-modeline" "jabber-modeline.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-modeline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-modeline" '("jabber-mode-line-")))

;;;***

;;;### (autoloads nil "jabber-muc" "jabber-muc.el" (0 0 0 0))
;;; Generated autoloads from jabber-muc.el

(defvar *jabber-active-groupchats* nil "\
alist of groupchats and nicknames
Keys are strings, the bare JID of the room.
Values are strings.")

(defvar jabber-muc-printers 'nil "\
List of functions that may be able to print part of a MUC message.
This gets prepended to `jabber-chat-printers', which see.")

(autoload 'jabber-muc-get-buffer "jabber-muc" "\
Return the chat buffer for chatroom GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP)" nil nil)

(autoload 'jabber-muc-private-get-buffer "jabber-muc" "\
Return the chat buffer for private chat with NICKNAME in GROUP.
Either a string or a buffer is returned, so use `get-buffer' or
`get-buffer-create'.

\(fn GROUP NICKNAME)" nil nil)

(autoload 'jabber-muc-vcard-get "jabber-muc" "\
Request vcard from chat with NICKNAME in GROUP.

\(fn JC GROUP NICKNAME)" t nil)

(autoload 'jabber-muc-message-p "jabber-muc" "\
Return non-nil if MESSAGE is a groupchat message.
That does not include private messages in a groupchat, but does
include groupchat invites.

\(fn MESSAGE)" nil nil)

(autoload 'jabber-muc-sender-p "jabber-muc" "\
Return non-nil if JID is a full JID of an MUC participant.

\(fn JID)" nil nil)

(autoload 'jabber-muc-private-message-p "jabber-muc" "\
Return non-nil if MESSAGE is a private message in a groupchat.

\(fn MESSAGE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-muc" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-muc-nick-coloring" "jabber-muc-nick-coloring.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-muc-nick-coloring.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-muc-nick-coloring" '("jabber-muc-")))

;;;***

;;;### (autoloads nil "jabber-muc-nick-completion" "jabber-muc-nick-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-muc-nick-completion.el

(autoload 'jabber-muc-looks-like-personal-p "jabber-muc-nick-completion" "\
Return non-nil if jabber MESSAGE is addresed to me.
Optional argument GROUP to look.

\(fn MESSAGE &optional GROUP)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-muc-nick-completion" '("try-expand-jabber-muc" "jabber-" "*jabber-muc-participant-last-speaking*")))

;;;***

;;;### (autoloads nil "jabber-newdisco" "jabber-newdisco.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-newdisco.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-newdisco" '("jabber-disco-")))

;;;***

;;;### (autoloads nil "jabber-ping" "jabber-ping.el" (0 0 0 0))
;;; Generated autoloads from jabber-ping.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ping" '("jabber-p")))

;;;***

;;;### (autoloads nil "jabber-presence" "jabber-presence.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-presence.el

(autoload 'jabber-send-presence "jabber-presence" "\
Set presence for all accounts.

\(fn SHOW STATUS PRIORITY)" t nil)

(autoload 'jabber-send-default-presence "jabber-presence" "\
Send default presence.
Default presence is specified by `jabber-default-show',
`jabber-default-status', and `jabber-default-priority'.

\(fn &optional IGNORE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-presence" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-private" "jabber-private.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-private.el

(autoload 'jabber-private-get "jabber-private" "\
Retrieve an item from private XML storage.
The item to retrieve is identified by NODE-NAME (a symbol) and
NAMESPACE (a string).

On success, SUCCESS-CALLBACK is called with JC and the retrieved
XML fragment.

On error, ERROR-CALLBACK is called with JC and the entire IQ
result.

\(fn JC NODE-NAME NAMESPACE SUCCESS-CALLBACK ERROR-CALLBACK)" nil nil)

(autoload 'jabber-private-set "jabber-private" "\
Store FRAGMENT in private XML storage.
SUCCESS-CALLBACK, SUCCESS-CLOSURE-DATA, ERROR-CALLBACK and
ERROR-CLOSURE-DATA are used as in `jabber-send-iq'.

\(fn JC FRAGMENT &optional SUCCESS-CALLBACK SUCCESS-CLOSURE-DATA ERROR-CALLBACK ERROR-CLOSURE-DATA)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-private" '("jabber-private-get-1")))

;;;***

;;;### (autoloads nil "jabber-ratpoison" "jabber-ratpoison.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-ratpoison.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-ratpoison" '("ratpoison" "jabber-ratpoison-message")))

;;;***

;;;### (autoloads nil "jabber-register" "jabber-register.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-register.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-register" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-roster" "jabber-roster.el" (0 0 0 0))
;;; Generated autoloads from jabber-roster.el

(autoload 'jabber-switch-to-roster-buffer "jabber-roster" "\
Switch to roster buffer.
Optional JC argument is ignored; it's there so this function can
be used in `jabber-post-connection-hooks'.

\(fn &optional JC)" t nil)

(autoload 'jabber-roster-update "jabber-roster" "\
Update roster, in memory and on display.
Add NEW-ITEMS, update CHANGED-ITEMS and remove DELETED-ITEMS, all
three being lists of JID symbols.

\(fn JC NEW-ITEMS CHANGED-ITEMS DELETED-ITEMS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-roster" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-sasl" "jabber-sasl.el" (0 0 0 0))
;;; Generated autoloads from jabber-sasl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-sasl" '("jabber-sasl-")))

;;;***

;;;### (autoloads nil "jabber-sawfish" "jabber-sawfish.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-sawfish.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-sawfish" '("sawfish" "jabber-sawfish-display-")))

;;;***

;;;### (autoloads nil "jabber-screen" "jabber-screen.el" (0 0 0 0))
;;; Generated autoloads from jabber-screen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-screen" '("screen" "jabber-screen-message")))

;;;***

;;;### (autoloads nil "jabber-search" "jabber-search.el" (0 0 0 0))
;;; Generated autoloads from jabber-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-search" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-si-client" "jabber-si-client.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-si-client.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-si-client" '("jabber-si-initiate")))

;;;***

;;;### (autoloads nil "jabber-si-common" "jabber-si-common.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-si-common.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-si-common" '("jabber-si-stream-methods")))

;;;***

;;;### (autoloads nil "jabber-si-server" "jabber-si-server.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from jabber-si-server.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-si-server" '("jabber-si-pro")))

;;;***

;;;### (autoloads nil "jabber-socks5" "jabber-socks5.el" (0 0 0 0))
;;; Generated autoloads from jabber-socks5.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-socks5" '("jabber-socks5")))

;;;***

;;;### (autoloads nil "jabber-time" "jabber-time.el" (0 0 0 0))
;;; Generated autoloads from jabber-time.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-time" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-tmux" "jabber-tmux.el" (0 0 0 0))
;;; Generated autoloads from jabber-tmux.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-tmux" '("tmux" "jabber-tmux-message")))

;;;***

;;;### (autoloads nil "jabber-truncate" "jabber-truncate.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-truncate.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-truncate" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-util" "jabber-util.el" (0 0 0 0))
;;; Generated autoloads from jabber-util.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-util" '("jabber-" "string>-numerical" "url-xmpp")))

;;;***

;;;### (autoloads nil "jabber-vcard" "jabber-vcard.el" (0 0 0 0))
;;; Generated autoloads from jabber-vcard.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-vcard" '("jabber-vcard-")))

;;;***

;;;### (autoloads nil "jabber-vcard-avatars" "jabber-vcard-avatars.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from jabber-vcard-avatars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-vcard-avatars" '("jabber-vcard-avatars-")))

;;;***

;;;### (autoloads nil "jabber-version" "jabber-version.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from jabber-version.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-version" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-watch" "jabber-watch.el" (0 0 0 0))
;;; Generated autoloads from jabber-watch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-watch" '("jabber-")))

;;;***

;;;### (autoloads nil "jabber-widget" "jabber-widget.el" (0 0 0 0))
;;; Generated autoloads from jabber-widget.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-widget" '("jid-complete" "jabber-")))

;;;***

;;;### (autoloads nil "jabber-wmii" "jabber-wmii.el" (0 0 0 0))
;;; Generated autoloads from jabber-wmii.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-wmii" '("wmii" "jabber-wmii-")))

;;;***

;;;### (autoloads nil "jabber-xmessage" "jabber-xmessage.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from jabber-xmessage.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-xmessage" '("xmessage" "jabber-xmessage-")))

;;;***

;;;### (autoloads nil "jabber-xml" "jabber-xml.el" (0 0 0 0))
;;; Generated autoloads from jabber-xml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "jabber-xml" '("jabber-")))

;;;***

;;;### (autoloads nil "srv" "srv.el" (0 0 0 0))
;;; Generated autoloads from srv.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "srv" '("srv-lookup")))

;;;***

;;;### (autoloads nil nil ("jabber-festival.el" "jabber-osd.el" "jabber-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jabber-autoloads.el ends here
