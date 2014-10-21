;;; identica-mode.el --- Major mode API client for status.net open microblogging

;; Copyright (C) 2008-2011 Gabriel Saldana
;; Copyright (C) 2009 Bradley M. Kuhn

;; Author: Gabriel Saldana <gsaldana@gmail.com>
;; Last update: 2011-10-20
;; Version: 1.3.1
;; Keywords: identica web
;; URL: http://blog.gabrielsaldana.org/identica-mode-for-emacs/
;; Contributors:
;;     Jason McBrayer <jmcbray@carcosa.net> (minor updates for working under Emacs 23)
;;     Alex Schröder <kensanata@gmail.com> (mode map patches)
;;     Christian Cheng (fixed long standing xml parsing bug)
;;     Carlos A. Perilla from denting-mode
;;     Alberto Garcia <agarcia@igalia.com> (integrated patch from twittering-mode for retrieving multiplemethods)
;;     Bradley M. Kuhn <bkuhn@ebb.org> (editing status from edit-buffer rather than minibuffer)
;;     Jason McBrayer <jmcbray@carcosa.net> (replace group tags with hashtags on redents, longlines use)
;;     Sean Neakums (patches of bugs flagged by byte-compiler)
;;     Shyam Karanatt <shyam@swathanthran.in> (several patches and code cleanup, new http backend based on url.el)
;;     Tezcatl Franco <tzk@riseup.net> (ur1.ca support)
;;     Anthony Garcia <lagg@lavabit.com> (fix for icon-mode)
;;     Alexande Oliva <lxoliva@fsfla.org> (fix for icon placement on reverse order dents, bug fixes)
;;     Aidan Gauland <aidalgol@no8wireless.co.nz> (variable scope code cleanup)
;;     Joel J. Adamson <adamsonj@email.unc.edu> Added countdown minibuffer-prompt style
;;     Kevin Granade <kevin.granade@gmail.com> (OAuth support)

;;; Commentary:

;; Identica Mode is a major mode to check friends timeline, and update your
;; status on Emacs.

;; identica-mode.el is a major mode for Identica.  Based on the twittering mode
;; version 0.6 by Y.  Hayamizu and Tsuyoshi CHO found at
;; <http://hayamin.com/wiliki.cgi?twittering-mode-en&l=en>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FORCouldn't findSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth floor,
;; Boston, MA 02110-1301, USA.

;; Requirements
;; if using Emacs22 or previous, you'll need json.el
;; get it from http://edward.oconnor.cx/2006/03/json.el
;; json.el is part of Emacs23
;; To use the OAuth support, you need oauth.el
;; Downloadable from http://github.com/psanford/emacs-oauth/

;; If using Oauth with Emacs earlier than 23.3 you'll also need w3m.

;;; Install:

;; You can use M-x customize-group identica-mode to setup all settings or simply
;; add the following to your .emacs or your prefered customizations file

;; (require 'identica-mode)
;; (setq identica-username "yourusername")

;; If you want to use simple authentication add your password
;; (setq identica-password "yourpassword")

;; It is recommended to create a file ~/.authinfo with your login credentials
;; instead of storing your password in plain text, the file should have the
;; following contents:

;; machine servername login yourusername password yourpassword

;; Replace servername with your server (if Identica server use identi.ca)
;; yourusername and yourpassword with your information. If you setup your
;; authinfo file, you don't need to set identica-password variable anywhere

;; If you want to use OAuth authentication add the following
;; (setq identica-auth-mode "oauth")

;; If you want to post from the minibufer without having identica buffer active, add the following global keybinding.
;; Add this to send status updates
;; (global-set-key "\C-cip" 'identica-update-status-interactive)
;; Add this to send direct messages
;; (global-set-key "\C-cid" 'identica-direct-message-interactive)

;; If you want to connect to a custom statusnet server add this and change
;; identi.ca with your server's doman name.

;; (setq statusnet-server "identi.ca")

;; Start using with M-x identica

;; Follow me on identica: http://identi.ca/gabrielsaldana

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)
(require 'url)
(require 'url-http)
(require 'json)
(require 'image)

(defconst identica-mode-version "1.3.1")

;;url-basepath fix for emacs22
(unless (fboundp 'url-basepath)
  (defalias 'url-basepath 'url-file-directory))

;;workaround for url-unhex-string bug that was fixed in emacs 23.3
(defvar identica-unhex-broken nil
  "Predicate indicating broken-ness of `url-unhex-string'.

If non-nil, indicates that `url-unhex-string' is broken and
must be worked around when using oauth.")

(defgroup identica-mode nil
  "Identica Mode for microblogging"
  :tag "Microblogging"
  :link '(url-link http://blog.gabrielsaldana.org/identica-mode-for-emacs/)
  :group 'applications )

(defun identica-mode-version ()
  "Display a message for identica-mode version."
  (interactive)
  (let ((version-string
         (format "identica-mode-v%s" identica-mode-version)))
    (if (interactive-p)
        (message "%s" version-string)
      version-string)))

(defvar identica-mode-map (make-sparse-keymap "Identi.ca"))
(defvar menu-bar-identica-mode-menu nil)
(defvar identica-timer nil "Timer object for timeline refreshing will be stored here.  DO NOT SET VALUE MANUALLY.")

(defvar identica-urlshortening-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl=")
    (google . "http://ggl-shortener.appspot.com/?url=")
    (ur1ca . "http://ur1.ca/?longurl=")
    (tighturl . "http://2tu.us/?save=y&url=")
    (isgd . "http://is.gd/create.php?format=simple&url="))
  "Alist of tinyfy services.")

(defvar identica-new-dents-count 0
  "Number of new tweets when `identica-new-dents-hook' is run.")

(defvar identica-new-dents-hook nil
  "Hook run when new twits are received.

You can read `identica-new-dents-count' to get the number of new
tweets received when this hook is run.")

(defvar identica-display-max-dents nil
  "How many dents to keep on the displayed timeline.

If non-nil, dents over this amount will bre removed.")

;; Menu
(unless menu-bar-identica-mode-menu
  (easy-menu-define
    menu-bar-identica-mode-menu identica-mode-map ""
    '("Identi.ca"
      ["Send an update" identica-update-status-interactive t]
      ["Send a direct message" identica-direct-message-interactive t]
      ["Re-dent someone's update" identica-redent t]
      ["Repeat someone's update" identica-repeat t]
      ["Add as favorite" identica-favorite t]
      ["Follow user" identica-follow]
      ["Unfollow user" identica-unfollow]
      ["--" nil nil]
      ["Friends timeline" identica-friends-timeline t]
      ["Public timeline" identica-public-timeline t]
      ["Replies timeline" identica-replies-timeline t]
      ["User timeline" identica-user-timeline t]
      ["Tag timeline" identica-tag-timeline t]
      ["--" nil nil]
      ;; ["Group timeline" identica-group-timeline t]
      ;; ["Join to this group" identica-group-join t]
      ["Leave this group" identica-group-leave t]
      )))

(defcustom identica-idle-time 20
  "Idle time."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-timer-interval 90
  "Timer interval to refresh the timeline."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-username nil
  "Your identi.ca username.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom identica-password nil
  "Your identi.ca password.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'identica-mode)

(defcustom identica-auth-mode "password"
  "Authorization mode used, options are password and oauth."
  :type 'string
  :group 'identica-mode)

(defun identica-enable-oauth ()
  "Enables oauth for identica-mode."
  (interactive)
  (require 'oauth)
                                        ;Test if we're running on an emacs version with broken unhex and apply workaround.
  (unless (eq (url-unhex-string (url-hexify-string "²")) "²")
    (setq identica-unhex-broken t)
    (require 'w3m))
  (setq identica-auth-mode "oauth"))

(defvar identica-mode-oauth-consumer-key
  "53e8e7bf7d1be8e58ef1024b31478d2b")

(defvar identica-mode-oauth-consumer-secret
  "1ab0876f14bd82c4eb450f720a0e84ae")

(defcustom statusnet-server "identi.ca"
  "Statusnet instance url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-request-url
  "https://identi.ca/api/oauth/request_token"
  "Statusnet oauth request_token url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-access-url
  "https://identi.ca/api/oauth/access_token"
  "Statusnet oauth access_token url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-authorize-url
  "https://identi.ca/api/oauth/authorize"
  "Statusnet authorization url."
  :type 'string
  :group 'identica-mode)

(defcustom statusnet-server-textlimit 140
  "Number of characters allowed in a status."
  :type 'integer
  :group 'identica-mode)

(defvar oauth-access-token nil)

(defcustom statusnet-port 80
  "Port on which StatusNet instance listens."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-default-timeline "friends_timeline"
  "Default timeline to retrieve."
  :type 'string
  :options '("friends_timeline" "public_timeline" "replies")
  :group 'identica-mode)

(defcustom identica-statuses-count 20
  "Default number of statuses to retrieve."
  :type 'integer
  :group 'identica-mode)

(defcustom identica-display-success-messages nil
  "Display messages when the timeline is successfully retrieved."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-oldest-first nil
  "If t, display older messages before newer ones."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-update-status-edit-confirm-cancellation nil
  "If t, ask user if they are sure when aborting editing of an
identica status update when using an edit-buffer"
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-soft-wrap-status t
  "If non-nil, don't fill status messages in the timeline as
paragraphs. Instead, use visual-line-mode or longlines-mode if
  available to wrap messages.  This may work better for narrow
  timeline windows."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-update-status-method 'minibuffer
  "Method for performaing status updates.

The available choices are:

  'minibuffer  - edit the status update in the minibuffer.
  'edit-buffer - edit the status update in an independent buffer."
  :type '(choice (const :tag "Edit status in minibuffer" minibuffer)
                 (const :tag "Edit status in independent buffer" edit-buffer))
  :group 'identica-mode)

(defcustom identica-http-get-timeout 10
  "Controls how long to wait for a response from the server."
  :type 'integer
  :group 'identica-mode)

;; Initialize with default timeline
(defvar identica-method identica-default-timeline)
(defvar identica-method-class "statuses")
(defvar identica-remote-server nil)

(defvar identica-scroll-mode nil)
(make-variable-buffer-local 'identica-scroll-mode)

(defvar identica-source "identica-mode")

(defcustom identica-redent-format "♻"
  "The format/symbol to represent redents."
  :type 'string
  :group 'identica-mode)

(defcustom identica-blacklist '()
  "List of regexes used to filter statuses, evaluated after status formatting is applied."
  :type 'string
  :group 'identica-mode)

(defcustom identica-status-format "%i %s,  %@:\n  %h%t // from %f%L%r\n\n"
  "The format used to display the status updates."
  :type 'string
  :group 'identica-mode)
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - in reply to status
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %h - favorited
;; %f - source
;; %# - id

(defcustom identica-urlshortening-service 'ur1ca
  "The service to use for URL shortening.
Values understood are ur1ca, tighturl, tinyurl, toly, google and isgd."
  :type 'symbol
  :group 'identica-mode)

(defvar identica-buffer "*identica*")
(defun identica-buffer (&optional method)
  "Create a buffer for use by identica-mode.
Initialize the global method with the default, or with METHOD, if present."
  (unless method
    (setq method "friends_timeline"))
  (get-buffer-create identica-buffer))

(defstruct (statusnet-oauth-data
            (:conc-name sn-oauth-))
  "The oauth configuration associated with a statusnet account."
  consumer-key ; string
  consumer-secret ; string
  request-url ; string
  access-url ; string
  authorize-url ; string
  access-token ; string
  )

(defstruct (statusnet-account
            (:conc-name sn-account-))
  "Container for account information."
  server ; string
  port ; integer
  username ; string
  auth-mode ; string, either "password" or "oauth"
  password ; string
  textlimit ; integer
  oauth-data ; statusnet-account-oauth-data
  last-timeline-retrieved ; string
  )

(defvar statusnet-accounts nil
  "A list of login credentials for statusnet instances.")

(defvar sn-current-account nil
  "A pointer to the statusnet account being processed.")

(defvar identica-http-buffer nil
  "Pointer to the current http response buffer.")

(defvar identica-timeline-data nil)
(defvar identica-timeline-last-update nil)
(defvar identica-highlighted-entries nil
  "List of entry ids selected for highlighting.")

(defcustom identica-enable-highlighting nil
  "If non-nil, set the background of every selected entry to the background
of identica-highlight-face."
  :type 'boolean
  :group 'identica-mode)

(defcustom identica-enable-striping nil
  "If non-nil, set the background of every second entry to the background
of identica-stripe-face."
  :type 'boolean
  :group 'identica-mode)

(defvar identica-username-face 'identica-username-face)
(defvar identica-uri-face 'identica-uri-face)
(defvar identica-reply-face 'identica-reply-face)
(defvar identica-stripe-face 'identica-stripe-face)
(defvar identica-highlight-face 'identica-highlight-face)

(defcustom identica-reply-bg-color "DarkSlateGray"
  "The background color on which replies are displayed."
  :type 'string
  :group 'identica-mode)

(defcustom identica-stripe-bg-color "SlateGray"
  "The background color on which striped entries are displayed."
  :type 'string
  :group 'identica-mode)

(defcustom identica-highlight-bg-color "DarkSlateGray"
  "The background color on which highlighted entries are displayed."
  :type 'string
  :group 'identica-mode)

;;; Proxy
(defvar identica-proxy-use nil)
(defvar identica-proxy-server nil)
(defvar identica-proxy-port 8080)
(defvar identica-proxy-user nil)
(defvar identica-proxy-password nil)

(defun identica-toggle-proxy ()
  "Toggle whether identica-mode uses a proxy."
  (interactive)
  (setq identica-proxy-use
        (not identica-proxy-use))
  (message "%s %s"
           "Use Proxy:"
           (if identica-proxy-use
               "on" "off")))

(defun identica-user-agent-default-function ()
  "Identica mode default User-Agent function."
  (concat "Emacs/"
          (int-to-string emacs-major-version) "." (int-to-string
                                                   emacs-minor-version)
          " "
          "Identica-mode/"
          identica-mode-version))

(defvar identica-user-agent-function 'identica-user-agent-default-function)

(defun identica-user-agent ()
  "Return User-Agent header string."
  (funcall identica-user-agent-function))

;;; to show image files

(defvar identica-tmp-dir
  (expand-file-name (concat "identicamode-images-" (user-login-name))
                    temporary-file-directory))

(defvar identica-icon-mode nil "You MUST NOT CHANGE this variable directory.  You should change through function'identica-icon-mode'.")
(make-variable-buffer-local 'identica-icon-mode)
(defun identica-icon-mode (&optional arg)
  (interactive)
  (setq identica-icon-mode
        (if identica-icon-mode
            (if (null arg)
                nil
              (> (prefix-numeric-value arg) 0))
          (when (or (null arg)
                    (and arg (> (prefix-numeric-value arg) 0)))
            (when (file-writable-p identica-tmp-dir)
              (progn
                (if (not (file-directory-p identica-tmp-dir))
                    (make-directory identica-tmp-dir))
                t)))))
  (identica-current-timeline))

(defun identica-scroll-mode (&optional arg)
  (interactive)
  (setq identica-scroll-mode
        (if (null arg)
            (not identica-scroll-mode)
          (> (prefix-numeric-value arg) 0))))

(defvar identica-image-stack nil)

(defun identica-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun identica-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
                      (apply 'encode-time (parse-time-string string))
                      uni))
(defun identica-local-strftime (fmt string)
  (identica-setftime fmt string nil))
(defun identica-global-strftime (fmt string)
  (identica-setftime fmt string t))

(defvar identica-debug-mode nil)
(defvar identica-debug-buffer "*identica-debug*")
(defun identica-debug-buffer ()
  (get-buffer-create identica-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if identica-debug-mode
           (with-current-buffer (identica-debug-buffer)
             (insert (prin1-to-string ,obsym))
             (newline)
             ,obsym)
         ,obsym))))

(defun identica-debug-mode ()
  (interactive)
  (setq identica-debug-mode
        (not identica-debug-mode))
  (message (if identica-debug-mode "debug mode:on" "debug mode:off")))

(defun identica-delete-notice ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
        (usern (get-text-property (point) 'username)))
    (if (string= usern (sn-account-username sn-current-account))
        (when (y-or-n-p "Delete this notice? ")
          (identica-http-post "statuses/destroy" (number-to-string id))
          (identica-get-timeline))
      (message "Can't delete a notice that isn't yours"))))

(if identica-mode-map
    (let ((km identica-mode-map))
      (define-key km "\C-c\C-f" 'identica-friends-timeline)
      ;;      (define-key km "\C-c\C-i" 'identica-direct-messages-timeline)
      (define-key km "\C-c\C-r" 'identica-replies-timeline)
      (define-key km "\C-c\C-a" 'identica-public-timeline)
      (define-key km "\C-c\C-g" 'identica-group-timeline)
      ;;      (define-ley km "\C-c\C-j" 'identica-group-join)
      ;;      (define-ley km "\C-c\C-l" 'identica-group-leave)
      (define-key km "\C-c\C-t" 'identica-tag-timeline)
      (define-key km "\C-c\C-k" 'identica-stop)
      (define-key km "\C-c\C-u" 'identica-user-timeline)
      (define-key km "\C-c\C-c" 'identica-conversation-timeline)
      (define-key km "\C-c\C-o" 'identica-remote-user-timeline)
      (define-key km "\C-c\C-s" 'identica-update-status-interactive)
      (define-key km "\C-c\C-d" 'identica-direct-message-interactive)
      (define-key km "\C-c\C-m" 'identica-redent)
      (define-key km "\C-c\C-h" 'identica-toggle-highlight)
      (define-key km "r" 'identica-repeat)
      (define-key km "F" 'identica-favorite)
      (define-key km "\C-c\C-e" 'identica-erase-old-statuses)
      (define-key km "\C-m" 'identica-enter)
      (define-key km "R" 'identica-reply-to-user)
      (define-key km "A" 'identica-reply-to-all)
      (define-key km "\t" 'identica-next-link)
      (define-key km [backtab] 'identica-prev-link)
      (define-key km [mouse-1] 'identica-click)
      (define-key km "\C-c\C-v" 'identica-view-user-page)
      (define-key km "q" 'bury-buffer)
      (define-key km "e" 'identica-expand-replace-at-point)
      (define-key km "j" 'identica-goto-next-status)
      (define-key km "k" 'identica-goto-previous-status)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'identica-goto-next-status-of-user)
      (define-key km "p" 'identica-goto-previous-status-of-user)
      (define-key km [backspace] 'scroll-down)
      (define-key km " " 'scroll-up)
      (define-key km "G" 'end-of-buffer)
      (define-key km "g" 'identica-current-timeline)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'identica-icon-mode)
      (define-key km "s" 'identica-scroll-mode)
      (define-key km "t" 'identica-toggle-proxy)
      (define-key km "\C-k" 'identica-delete-notice)
      (define-key km "\C-c\C-p" 'identica-toggle-proxy)
      nil))

(defvar identica-mode-syntax-table nil "")

(if identica-mode-syntax-table
    ()
  (setq identica-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" identica-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  identica-mode-syntax-table))

(defun identica-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (make-variable-buffer-local 'identica-active-mode)
  (set-default 'identica-active-mode t)
  (font-lock-mode -1)
  (defface identica-username-face
    `((t nil)) "" :group 'faces)
  (defface identica-reply-face
    `((t nil)) "" :group 'faces)
  (defface identica-stripe-face
    `((t nil)) "" :group 'faces)
  (defface identica-highlight-face
    `((t nil)) "" :group 'faces)
  (defface identica-uri-face
    `((t nil)) "" :group 'faces)
  (defface identica-heart-face
    `((t nil)) "" :group 'faces)

  (add-to-list 'minor-mode-alist '(identica-icon-mode " id-icon"))
  (add-to-list 'minor-mode-alist '(identica-scroll-mode " id-scroll"))

  ;; make face properties nonsticky
  (unless (boundp 'identica-text-property-nonsticky-adjustment)
    (setq identica-text-property-nonsticky-adjustment t)
    (nconc text-property-default-nonsticky
           '((face . t)(mouse-face . t)(uri . t)(source . t)(uri-in-text . t))))

  (identica-create-account))

(defun identica-create-account ()
  "Create an account object based on the various custom variables.
  Insert it into the statusnet accounts list.
This needs to be called from any globally-accessable entry point."
  (unless (boundp 'statusnet-account-created)
    (setq statusnet-account-created t)
    (setq statusnet-accounts
          (cons (make-statusnet-account
                 :server statusnet-server
                 :port statusnet-port
                 :username identica-username
                 :auth-mode identica-auth-mode
                 :password identica-password
                 :textlimit statusnet-server-textlimit
                 :oauth-data (if (string= identica-auth-mode "oauth")
                                 (make-statusnet-oauth-data
                                  :consumer-key identica-mode-oauth-consumer-key
                                  :consumer-secret identica-mode-oauth-consumer-secret
                                  :request-url statusnet-request-url
                                  :access-url statusnet-access-url
                                  :authorize-url statusnet-authorize-url
                                  :access-token nil)
                               nil)
                 :last-timeline-retrieved nil)
                statusnet-accounts))
    (setq sn-current-account (car statusnet-accounts))))

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
         (let ((keylist (car clause))
               (body (cdr clause)))
           `(,(if (listp keylist)
                  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key)) keylist))
                't)
             ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro identica-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar identica-mode-string identica-method)

(defun identica-set-mode-string (loading)
  (with-current-buffer (identica-buffer)
    (let ((timeline-url
           (concat (or identica-remote-server
                       (sn-account-server sn-current-account))
                   "/" identica-method)))
      (setq mode-name
            (if loading (concat
                         (if (stringp loading) loading "loading")
                         " " timeline-url "...")
              timeline-url))
      (debug-print mode-name))))

(defvar identica-mode-hook nil
  "Identica-mode hook.")

(defcustom identica-load-hook nil
  "Hook that is run after identica-mode.el has been loaded."
  :group 'identica-mode
  :type 'hook)

(defun identica-kill-buffer-function ()
  (when (eq major-mode 'identica-mode)
    (identica-stop)))

(defun identica-autoload-oauth ()
  "Autoloads oauth.el when needed."
  (autoload 'oauth-authorize-app "oauth")
  (autoload 'oauth-hexify-string "oauth")
  (autoload 'make-oauth-access-token "oauth"))

(defun identica-mode ()
  "Major mode for Identica.
  \\{identica-mode-map}"
  (interactive)
  (identica-autoload-oauth)
  (switch-to-buffer (identica-buffer))
  (buffer-disable-undo (identica-buffer))
  (kill-all-local-variables)
  (identica-mode-init-variables)
  (use-local-map identica-mode-map)
  (setq major-mode 'identica-mode)
  (setq mode-name identica-mode-string)
  (setq mode-line-buffer-identification
        `(,(default-value 'mode-line-buffer-identification)
          (:eval (identica-mode-line-buffer-identification))))
  (identica-update-mode-line)
  (set-syntax-table identica-mode-syntax-table)
  (font-lock-mode -1)
  (if identica-soft-wrap-status
      (if (fboundp 'visual-line-mode)
          (visual-line-mode t)
        (if (fboundp 'longlines-mode)
            (longlines-mode t))))
  (identica-retrieve-configuration)
  (add-hook 'kill-buffer-hook 'identica-kill-buffer-function)
  (run-mode-hooks 'identica-mode-hook))

;;;
;;; Basic HTTP functions
;;;

(defun identica-set-proxy (&optional url username passwd server port)
  "Sets the proxy authentication variables as required by url library.
When called with no arguments, it reads `identica-mode' proxy
variables to get the authentication parameters.URL is either a string
or parsed URL.  If URL is non-nil and valid, proxy authentication
values are read from it.  The rest of the arguments can be used to
directly set proxy authentication.  This function essentially adds
authentication parameters from one of the above methods to the double
alist `url-http-proxy-basic-auth-storage' and sets `url-using-proxy'."
  (let* ((href (if (stringp url)
                   (url-generic-parse-url url)
                 url))
         (port (or (and href (url-port href))
                   port identica-proxy-port))
         (port (if (integerp port) (int-to-string port) port))
         (server (or (and href (url-host href))
                     server identica-proxy-server))
         (server (and server
                      (concat server (when port (concat ":" port)))))
         (file (if href (let ((file-url (url-filename href)))
                          (cond
                           ((string= "" file-url) "/")
                           ((string-match "/$" file-url) file-url)
                           (t (url-basepath file-url))))
                 "Proxy"))
         (password (or (and href (url-password href))
                       passwd identica-proxy-password))
         (auth (concat (or (and href (url-user href))
                           username identica-proxy-user)
                       (and password (concat ":" password)))))
    (when (and identica-proxy-use
               (not (string= "" server))
               (not (string= "" auth)))
      (setq url-using-proxy server)
      (let* ((proxy-double-alist
              (or (assoc server
                         url-http-proxy-basic-auth-storage)
                  (car (push (cons server nil)
                             url-http-proxy-basic-auth-storage))))
             (proxy-auth-alist (assoc file proxy-double-alist)))
        (if proxy-auth-alist
            (setcdr proxy-auth-alist (base64-encode-string auth))
          (setcdr proxy-double-alist
                  (cons (cons file
                              (base64-encode-string auth))
                        (cdr-safe proxy-double-alist))))))))

(defun identica-change-user ()
  (interactive)
  "Interactive function to instantly change user authentication.
Directly reads parameters from user.  This function only sets the
identica-mode variables `(sn-account-username sn-current-account)' and
`(sn-account-password sn-current-account)'.
It is the `identica-set-auth' function that eventually sets the
url library variables according to the above variables which does the
authentication.  This will be done automatically in normal use cases
enabling dynamic change of user authentication."
  (interactive)
  (identica-ask-credentials)
  (identica-get-timeline))

(defun identica-ask-credentials ()
  "Asks for your username and password."
  (setf (sn-account-username sn-current-account)
        (read-string (concat "Username [for " (sn-account-server sn-current-account)
                             ":" (int-to-string (sn-account-port sn-current-account)) "]: ")
                     nil nil (sn-account-username sn-current-account))
        (sn-account-password sn-current-account)
        (read-passwd "Password: " nil (sn-account-password sn-current-account))))

(defun identica-set-auth (&optional url username passwd server port)
  "Sets the authentication parameters as required by url library.
If URL is non-nil and valid, it reads user authentication
parameters from url.  If URL is nil, Rest of the arguments can be
used to directly set user authentication.
When called with no arguments, user authentication parameters are
read from identica-mode variables `(sn-account-username sn-current-account)'
`(sn-account-password sn-current-account)' `(sn-account-server sn-current-account)'
 `(sn-account-port sn-current-account)'.
The username and password can also be set on ~/.authinfo,
~/.netrc or ~/.authinfo.gpg files for better security.
In this case `(sn-account-password sn-current-account)' should
not be predefined in any .emacs or init.el files, only
`(sn-account-username sn-current-account)' should be set."
  (unless (sn-account-username sn-current-account)
    (identica-ask-credentials))
  (let* ((href (if (stringp url)
                   (url-generic-parse-url url)
                 url))
         (port (or (and href (url-port href))
                   port (sn-account-port sn-current-account)))
         (port (if (integerp port) (int-to-string port) port))
         (server (or (and href (url-host href))
                     server (sn-account-server sn-current-account)))
         (servername server)
         (server (and server
                      (concat server (when port (concat ":" port)))))
         (file (if href (let ((file-url (url-filename href)))
                          (cond
                           ((string= "" file-url) "/")
                           ((string-match "/$" file-url) file-url)
                           (t (url-basepath file-url))))
                 "Identi.ca API"))

         (auth-user (if (functionp 'auth-source-search)
                        (plist-get (car (auth-source-search :host servername :max 1)) :user)
                      (auth-source-user-or-password "login" server "http")))
         (auth-pass (if (functionp 'auth-source-search)
                        (if (functionp (plist-get (car (auth-source-search :host servername :max 1)) :secret))
                            (funcall (plist-get (car (auth-source-search :host servername :max 1)) :secret))
                          (plist-get (car (auth-source-search :host servername :max 1)) :secret))
                      (auth-source-user-or-password "password" server "http")))
         (password (or auth-pass (and href (url-password href))
                       passwd (sn-account-password sn-current-account)))
         (auth (concat (or auth-user (and href (url-user href))
                           username (sn-account-username sn-current-account))
                       (and password (concat ":" password)))))
    (when (and (not (string= "" server))
               (not (string= "" auth)))
      (let* ((server-double-alist
              (or (assoc server
                         url-http-real-basic-auth-storage)
                  (car (push (cons server nil)
                             url-http-real-basic-auth-storage))))
             (api-auth-alist (assoc file server-double-alist)))
        (if api-auth-alist
            (setcdr api-auth-alist (base64-encode-string auth))
          (setcdr server-double-alist
                  (cons (cons file
                              (base64-encode-string auth))
                        (cdr-safe server-double-alist))))))))

(defun identica-initialize-oauth ()
  "Get authentication token unless we have one stashed already.
Shamelessly stolen from yammer.el"
  (let ((filename (concat "~/." (sn-account-server sn-current-account) "-"
                          (sn-account-username sn-current-account) "-oauth-token")))
    (when (file-exists-p filename)
      (save-excursion
        (find-file filename)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)"
                            (buffer-substring (point-min) (point-max)))
              (setf (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
                    (make-oauth-access-token
                     :consumer-key (sn-oauth-consumer-key (sn-account-oauth-data sn-current-account))
                     :consumer-secret (sn-oauth-consumer-secret (sn-account-oauth-data sn-current-account))
                     :auth-t (make-oauth-t
                              :token (match-string 1 str)
                              :token-secret (match-string 2 str))))))
        (save-buffer)
        (kill-this-buffer)))
    (unless (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
      (setf (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
            (oauth-authorize-app (sn-oauth-consumer-key (sn-account-oauth-data sn-current-account))
                                 (sn-oauth-consumer-secret (sn-account-oauth-data sn-current-account))
                                 (sn-oauth-request-url (sn-account-oauth-data sn-current-account))
                                 (sn-oauth-access-url (sn-account-oauth-data sn-current-account))
                                 (sn-oauth-authorize-url (sn-account-oauth-data sn-current-account))))
      (save-excursion
        (find-file filename)
        (end-of-buffer)
        (let ((token (oauth-access-token-auth-t (sn-oauth-access-token (sn-account-oauth-data sn-current-account)))))
          (insert (format "%s:%s\n"
                          (oauth-t-token token)
                          (oauth-t-token-secret token))))
        (save-buffer)
        (kill-this-buffer))))
  (sn-oauth-access-token (sn-account-oauth-data sn-current-account)))

(defun identica-http-get
  (server auth-mode method-class method &optional parameters sentinel sentinel-arguments)
  "Basic function which communicates with server.
METHOD-CLASS and METHOD are parameters for getting dents messages and
other information from SERVER as specified in api documentation.
Third optional arguments specify the additional parameters required by
the above METHOD.  It is specified as an alist with parameter name and
its corresponding value SENTINEL represents the callback function to
be called after the http response is completely retrieved.
SENTINEL-ARGUMENTS is the list of arguments (if any) of the SENTINEL
procedure."
  (or sentinel (setq sentinel 'identica-http-get-default-sentinel))
  (let ((url (concat "http://" server "/api/"
                     (when (not (string-equal method-class "none"))
                       (concat method-class "/" ))
                     method ".xml"
                     (when parameters
                       (concat "?"
                               (mapconcat
                                (lambda (param-pair)
                                  (format "%s=%s"
                                          (identica-percent-encode (car param-pair))
                                          (identica-percent-encode (cdr param-pair))))
                                parameters
                                "&")))))
        (url-package-name "emacs-identica-mode")
        (url-package-version identica-mode-version)
        (url-show-status nil))
    (identica-set-proxy)
    (unless (equal auth-mode "none")
      (if (equal auth-mode "oauth")
          (or (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
              (identica-initialize-oauth))
        (identica-set-auth url)))
    (when (get-buffer-process identica-http-buffer)
      (delete-process identica-http-buffer)
      (kill-buffer identica-http-buffer))
    (setq identica-http-buffer
          (identica-url-retrieve url sentinel method-class
                                 method parameters sentinel-arguments auth-mode))
    (set-buffer identica-buffer)
    (identica-set-mode-string t)))

(defun identica-render-pending-dents ()
  (interactive)
  "If at the time an HTTP request for new dents finishes,
identica-buffer is not active, we defer its update, to make sure
we adjust point within the right frame."
  (identica-render-timeline)
  (when (> identica-new-dents-count 0)
    (run-hooks 'identica-new-dents-hook)
    (setq identica-new-dents-count 0))
  (when identica-display-success-messages
    (message "Success: Get")))

(defun identica-http-get-default-sentinel
  (&optional status method-class method parameters success-message)
  (debug-print (window-buffer))
  (let ((error-object (assoc-workaround :error status))
        (active-p (eq (window-buffer) (identica-buffer))))
    (cond (error-object
           (let ((error-data (format "%s" (caddr error-object))))
             (when (cond
                    ((string= error-data "deleted\n") t)
                    ((and (string= error-data "404") method
                          (= 13 (string-match "/" method)))
                     (message "No Such User: %s" (substring method 14))
                     t)
                    ((y-or-n-p
                      (format "Identica-Mode: Network error:%s Retry? "
                              status))
                     (identica-http-get (sn-account-server sn-current-account)
                                        (sn-account-auth-mode sn-current-account)
                                        method-class method parameters)
                     nil))
               ;; when the network process is deleted by another query
               ;; or the user queried is not found , query is _finished_
               ;; unsuccessful and we want to restore identica-method
               ;; to loose track of this unsuccessful attempt
               (setq identica-method (sn-account-last-timeline-retrieved sn-current-account)))))
          ((< (- (point-max) (or (re-search-forward ">\r?\n\r*$" nil t) 0)) 2)
           ;;Checking the whether the message is complete by
           ;;searching for > that closes the last tag, followed by
           ;;CRLF at (point-max)
           (let ((body (identica-get-response-body)))
             (if (not body)
                 (identica-set-mode-string nil)
               (setq identica-new-dents-count
                     (+ identica-new-dents-count
                        (count t (mapcar
                                  #'identica-cache-status-datum
                                  (reverse (identica-xmltree-to-status
                                            body))))))
                                        ; Shorten the timeline if necessary
               (if (and identica-display-max-dents
                        (> (safe-length identica-timeline-data)
                           identica-display-max-dents))
                   (cl-set-nthcdr identica-display-max-dents
                                  identica-timeline-data nil))
               (if active-p
                   (identica-render-pending-dents)
                 (identica-set-mode-string "pending"))))))))

(defun merge-text-attribute (start end new-face attribute)
  "Merge the ATTRIBUTE of NEW-FACE into the text between START and END.
If we just add the new face its attributes somehow get overridden by
the attributes of the underlying face, so instead we just add the attribute
we are interested in."
  (while (not (eq start end))
    (let ((bg (face-attribute new-face attribute))
          (prop (get-text-property start 'face))
          (next-change
           (next-single-property-change start 'face (current-buffer) end)))
      (if prop
          (add-text-properties start next-change
                               (list 'face
                                     (list prop
                                           (list attribute bg))))
        (add-text-properties start next-change
                             (list 'face (list attribute bg))))
      (setq start next-change))))

(defun identica-render-timeline ()
  (with-current-buffer (identica-buffer)
    (set-face-attribute 'identica-username-face nil
                        :underline t)
    (set-face-attribute 'identica-reply-face nil
                        :background identica-reply-bg-color)
    (set-face-attribute 'identica-stripe-face nil
                        :background identica-stripe-bg-color)
    (set-face-attribute 'identica-highlight-face nil
                        :background identica-highlight-bg-color)
    (set-face-attribute 'identica-uri-face nil
                        :underline t)
    (set-face-attribute 'identica-heart-face nil
                        :foreground "firebrick1" :height 2.0)
    (let ((point (point))
          (end (point-max))
          (wrapped (cond (visual-line-mode 'visual-line-mode)
                         (longlines-mode 'longlines-mode)
                         (t nil)))
          (stripe-entry nil))

      (setq buffer-read-only nil)
      (erase-buffer)
      (when wrapped (funcall wrapped -1))
      (mapc (lambda (status)
              (let ((before-status (point-marker))
                    (blacklisted 'nil)
                    (formatted-status (identica-format-status
                                       status identica-status-format)))
                (mapc (lambda (regex)
                        (when (string-match-p regex formatted-status)
                          (setq blacklisted 't)))
                      identica-blacklist)
                (unless blacklisted
                  (when identica-enable-striping
                    (setq stripe-entry (not stripe-entry)))
                  (insert formatted-status)
                  (when (not wrapped)
                    (fill-region-as-paragraph
                     (save-excursion (beginning-of-line -1) (point)) (point)))
                  (insert-and-inherit "\n")
                  ;; Apply highlight overlays to status
                  (when (or (string-equal (sn-account-username sn-current-account)
                                          (assoc-default 'in-reply-to-screen-name status))
                            (string-match
                             (concat "@" (sn-account-username sn-current-account)
                                     "\\([^[:word:]_-]\\|$\\)") (assoc-default 'text status)))
                    (merge-text-attribute before-status (point) 'identica-reply-face :background))
                  (when (and identica-enable-highlighting
                             (memq (assoc-default 'id status) identica-highlighted-entries))
                    (merge-text-attribute before-status (point) 'identica-highlight-face :background))
                  (when stripe-entry
                    (merge-text-attribute before-status (point) 'identica-stripe-face :background))
                  (when identica-oldest-first (goto-char (point-min))))))
            identica-timeline-data)
      (when (and identica-image-stack window-system) (clear-image-cache))
      (when wrapped (funcall wrapped 1))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if identica-scroll-mode (- (point-max) end) 0)))
      (identica-set-mode-string nil)
      (setf (sn-account-last-timeline-retrieved sn-current-account) identica-method)
      (if transient-mark-mode (deactivate-mark)))))

(defun identica-format-status (status format-str)
  (flet ((attr (key)
               (assoc-default key status))
         (profile-image
          ()
          (let ((profile-image-url (attr 'user-profile-image-url)))
            (when (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
              (let ((filename (match-string-no-properties 1 profile-image-url))
                    (xfilename (match-string-no-properties 0 profile-image-url)))
                ;; download icons if does not exist
                (unless (file-exists-p (concat identica-tmp-dir filename))
                  (if (file-exists-p (concat identica-tmp-dir xfilename))
                      (setq filename xfilename)
                    (setq filename nil)
                    (add-to-list 'identica-image-stack profile-image-url)))
                (when (and identica-icon-mode filename)
                  (let ((avatar (create-image (concat identica-tmp-dir filename))))
                    ;; Make sure the avatar is 48 pixels (which it should already be!, but hey...)
                    ;; For offenders, the top left slice of 48 by 48 pixels is displayed
                    ;; TODO: perhaps make this configurable?
                    (insert-image avatar nil nil `(0 0 48 48)))
                  nil))))))
    (let ((cursor 0)
          (result ())
          c
          found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)" format-str cursor))
        (setq c (string-to-char (match-string-no-properties 1 format-str)))
        (if (> found-at cursor)
            (push (substring format-str cursor found-at) result)
          "|")
        (setq cursor (match-end 1))

        (case c
          ((?s)                         ; %s - screen_name
           (push (attr 'user-screen-name) result))
          ((?S)                         ; %S - name
           (push (attr 'user-name) result))
          ((?i)                         ; %i - profile_image
           (push (profile-image) result))
          ((?d)                         ; %d - description
           (push (attr 'user-description) result))
          ((?l)                         ; %l - location
           (push (attr 'user-location) result))
          ((?L)                         ; %L - " [location]"
           (let ((location (attr 'user-location)))
             (unless (or (null location) (string= "" location))
               (push (concat " [" location "]") result)) ))
          ((?u)                         ; %u - url
           (push (attr 'user-url) result))
          ((?U)                         ; %U - profile url
           (push (cadr (split-string (attr 'user-profile-url) "https*://")) result))
          ((?j)                         ; %j - user.id
           (push (format "%d" (attr 'user-id)) result))
          ((?r)                         ; %r - in_reply_to_status_id
           (let ((reply-id (attr 'in-reply-to-status-id))
                 (reply-name (attr 'in-reply-to-screen-name)))
             (unless (or (null reply-id) (string= "" reply-id)
                         (null reply-name) (string= "" reply-name))
               (let ((in-reply-to-string (format "in reply to %s" reply-name))
                     (url (identica-get-status-url reply-id)))
                 (add-text-properties
                  0 (length in-reply-to-string)
                  `(mouse-face highlight
                               face identica-uri-face
                               uri ,url)
                  in-reply-to-string)
                 (push (concat " " in-reply-to-string) result)))))
          ((?p)                         ; %p - protected?
           (let ((protected (attr 'user-protected)))
             (when (string= "true" protected)
               (push "[x]" result))))
          ((?c)                     ; %c - created_at (raw UTC string)
           (push (attr 'created-at) result))
          ((?C) ; %C{time-format-str} - created_at (formatted with time-format-str)
           (push (identica-local-strftime
                  (or (match-string-no-properties 2 format-str) "%H:%M:%S")
                  (attr 'created-at))
                 result))
          ((?@)                         ; %@ - X seconds ago
           (let ((created-at
                  (apply
                   'encode-time
                   (parse-time-string (attr 'created-at))))
                 (now (current-time)))
             (let ((secs (+ (* (- (car now) (car created-at)) 65536)
                            (- (cadr now) (cadr created-at))))
                   time-string url)
               (setq time-string
                     (cond ((< secs 5) "less than 5 seconds ago")
                           ((< secs 10) "less than 10 seconds ago")
                           ((< secs 20) "less than 20 seconds ago")
                           ((< secs 30) "half a minute ago")
                           ((< secs 60) "less than a minute ago")
                           ((< secs 150) "1 minute ago")
                           ((< secs 2400) (format "%d minutes ago"
                                                  (/ (+ secs 30) 60)))
                           ((< secs 5400) "about 1 hour ago")
                           ((< secs 84600) (format "about %d hours ago"
                                                   (/ (+ secs 1800) 3600)))
                           (t (format-time-string "%I:%M %p %B %d, %Y" created-at))))
               (setq url (identica-get-status-url (attr 'id)))
               ;; make status url clickable
               (add-text-properties
                0 (length time-string)
                `(mouse-face highlight
                             face identica-uri-face
                             uri ,url)
                time-string)
               (push time-string result))))
          ((?t)                         ; %t - text
           (push                   ;(clickable-text)
            (attr 'text)
            result))
          ((?')                         ; %' - truncated
           (let ((truncated (attr 'truncated)))
             (when (string= "true" truncated)
               (push "..." result))))
          ((?f)                         ; %f - source
           (push (attr 'source) result))
          ((?F)                         ; %F - ostatus-aware source
           (push (if (string= (attr 'source) "ostatus")
                     (cadr (split-string (attr 'user-profile-url) "https*://"))
                   (attr 'source)) result))
          ((?#)                         ; %# - id
           (push (format "%d" (attr 'id)) result))
          ((?x)                         ; %x - conversation id (conteXt) - default 0
           (push (attr 'conversation-id) result))
          ((?h)
           (let ((likes (attr 'favorited)))
             (when (string= "true" likes)
               (push (propertize "❤" 'face 'identica-heart-face) result))))
          (t
           (push (char-to-string c) result))))
      (push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
        (add-text-properties 0 (length formatted-status)
                             `(username, (attr 'user-screen-name)
                                         id, (attr 'id)
                                         text, (attr 'text)
                                         profile-url, (attr 'user-profile-url)
                                         conversation-id, (attr 'conversation-id))
                             formatted-status)
        formatted-status))))

(defun identica-url-retrieve
  (url sentinel method-class method parameters sentinel-arguments &optional auth-mode unhex-workaround)
  "Call url-retrieve or oauth-url-retrieve dsepending on the mode.
Apply url-unhex-string workaround if necessary."
  (if (and (equal auth-mode "oauth")
           (sn-oauth-access-token (sn-account-oauth-data sn-current-account)))
      (if unhex-workaround
          (flet ((oauth-extract-url-params
                  (req)
                  "Modified oauth-extract-url-params using w3m-url-decode-string to work around
bug in url-unhex-string present in emacsen previous to 23.3."
                  (let ((url (oauth-request-url req)))
                    (when (string-match (regexp-quote "?") url)
                      (mapcar (lambda (pair)
                                `(,(car pair) . ,(w3m-url-decode-string (cadr pair))))
                              (url-parse-query-string (substring url (match-end 0))))))))
            (identica-url-retrieve url sentinel method-class method parameters sentinel-arguments auth-mode))
        (oauth-url-retrieve (sn-oauth-access-token (sn-account-oauth-data sn-current-account)) url sentinel
                            (append (list method-class method parameters)
                                    sentinel-arguments)))
    (url-retrieve url sentinel
                  (append (list method-class method parameters)
                          sentinel-arguments))))

(defun identica-http-post
  (method-class method &optional parameters sentinel sentinel-arguments)
  "Send HTTP POST request to statusnet server.
METHOD-CLASS must be one of Identica API method classes(statuses, users or direct_messages).
METHOD must be one of Identica API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (or sentinel (setq sentinel 'identica-http-post-default-sentinel))
  (let ((url-request-method "POST")
        (url (concat "http://"(sn-account-server sn-current-account) "/api/" method-class "/" method ".xml"
                     (when parameters
                       (concat "?"
                               (mapconcat
                                (lambda (param-pair)
                                  (format "%s=%s"
                                          (identica-percent-encode (car param-pair))
                                          (identica-percent-encode (cdr param-pair))))
                                parameters
                                "&")))))
        (url-package-name "emacs-identicamode")
        (url-package-version identica-mode-version)
        ;; (if (assoc `media parameters)
        ;; (url-request-extra-headers '(("Content-Type" . "multipart/form-data")))
        (url-request-extra-headers '(("Content-Length" . "0")))
        (url-show-status nil))
    (identica-set-proxy)
    (if (equal (sn-account-auth-mode sn-current-account) "oauth")
        (or (sn-oauth-access-token (sn-account-oauth-data sn-current-account))
            (identica-initialize-oauth))
      (identica-set-auth url))
    (when (get-buffer-process identica-http-buffer)
      (delete-process identica-http-buffer)
      (kill-buffer identica-http-buffer))
    (identica-url-retrieve url sentinel method-class method parameters
                           sentinel-arguments (sn-account-auth-mode sn-current-account) identica-unhex-broken)))

(defun identica-http-post-default-sentinel
  (&optional status method-class method parameters success-message)
  (let ((error-object (assoc-workaround :error status)))
    (cond  ((and
             error-object
             (y-or-n-p (format "Network error:%s %s Retry? "
                               (cadr error-object)
                               (caddr error-object))))
            (identica-http-post method-class method parameters nil success-message))
           (identica-display-success-messages
            (message (or success-message "Success: Post")))))
  (unless (get-buffer-process (current-buffer))
    (kill-buffer (current-buffer))))

(defun identica-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer.
 If BUFFER is omitted, 'current-buffer' is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (let ((end (or (and (search-forward-regexp "\r?\n\r?\n" (point-max) t)
                      (match-beginning 0))
                 0)))
    (and (> end 1)
         (buffer-substring (point-min) end))))

(defun identica-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, current-buffer is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (set-buffer-multibyte t)
  (let ((start (save-excursion
                 (goto-char (point-min))
                 (and (re-search-forward "<\\?xml" (point-max) t)
                      (match-beginning 0)))))
    (identica-clean-response-body)
    (and start
         (prog1
             (xml-parse-region start (point-max))
           (if identica-debug-mode
               t
             (kill-buffer buffer))))))

(defun identica-clean-weird-chars (&optional buffer)
  (with-current-buffer identica-http-buffer
    (goto-char (point-min))
    (while (re-search-forward "\

?
[0-9a-z]*\

?
?" nil t)
(replace-match ""))
(buffer-string)))

(defun identica-clean-response-body ()
  "Remove weird strings (e.g., 1afc, a or 0) from the response body.
Known Statusnet issue.  Mostly harmless except if in tags."
  (goto-char (point-min))
  (while (re-search-forward "\r?\n[0-9a-z]+\r?\n" nil t)
    (replace-match "")))

(defun identica-compare-statuses (a b)
  "Compare a pair of statuses.
For use as a predicate for sort."
  (< (assoc-default 'id b) (assoc-default 'id a)))

(defun identica-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default `identica-timeline-data')
If STATUS-DATUM is already in DATA-VAR, return nil.  If not, return t."
  (when (null data-var)
    (setf data-var 'identica-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
            (not (find-if
                  (lambda (item)
                    (eql id (cdr (assq 'id item))))
                  (symbol-value data-var))))
        (progn
          (set data-var (sort (cons status-datum (symbol-value data-var))
                              'identica-compare-statuses))
          t)
      nil)))

(defun identica-status-to-status-datum (status)
  (flet ((assq-get (item seq)
                   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
           id text source created-at truncated favorited
           in-reply-to-status-id
           in-reply-to-screen-name
           (user-data (cddr (assq 'user status-data)))
           user-id user-name
           conversation-id
           user-screen-name
           user-location
           user-description
           user-profile-image-url
           user-profile-url
           user-url
           user-protected
           regex-index)

      (setq id (string-to-number (assq-get 'id status-data)))
      (setq text (identica-decode-html-entities
                  (assq-get 'text status-data)))
      (setq source (identica-decode-html-entities
                    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq favorited (assq-get 'favorited status-data))
      (setq in-reply-to-status-id
            (identica-decode-html-entities
             (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
            (identica-decode-html-entities
             (assq-get 'in_reply_to_screen_name status-data)))
      (setq conversation-id (or (assq-get 'statusnet:conversation_id status-data) "0"))
      (setq user-id (string-to-number (assq-get 'id user-data)))
      (setq user-name (identica-decode-html-entities
                       (assq-get 'name user-data)))
      (setq user-screen-name (identica-decode-html-entities
                              (assq-get 'screen_name user-data)))
      (setq user-location (identica-decode-html-entities
                           (assq-get 'location user-data)))
      (setq user-description (identica-decode-html-entities
                              (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))
      (setq user-profile-url (assq-get 'statusnet:profile_url user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
                    uri ,user-profile-url
                    face identica-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
                    face identica-username-face
                    uri ,user-profile-url
                    face identica-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
        (setq regex-index
              (string-match "@\\([_[:word:]0-9]+\\)\\|!\\([_[:word:]0-9\-]+\\)\\|#\\([_[:word:]0-9\-]+\\)\\|\\(ur1\.ca/[a-z0-9]+/?\\|https?://[-_.!~*'()[:word:]0-9\;/?:@&=+$,%#]+\\)"
                            text
                            regex-index))
        (when regex-index
          (let* ((matched-string (match-string-no-properties 0 text))
                 (screen-name (match-string-no-properties 1 text))
                 (group-name (match-string-no-properties 2 text))
                 (tag-name (match-string-no-properties 3 text))
                 (uri (match-string-no-properties 4 text)))
            (add-text-properties
             (if (or screen-name group-name tag-name)
                 (+ 1 (match-beginning 0))
               (match-beginning 0))
             (match-end 0)
             (if (or screen-name group-name tag-name)
                 `(mouse-face
                   highlight
                   face identica-uri-face
                   uri ,(if screen-name
                            (concat "https://" (sn-account-server sn-current-account) "/" screen-name)
                          (if group-name
                              (concat "https://" (sn-account-server sn-current-account) "/group/" group-name)
                            (concat "https://" (sn-account-server sn-current-account) "/tag/" tag-name)))
                   uri-in-text ,(if screen-name
                                    (concat "https://" (sn-account-server sn-current-account) "/" screen-name)
                                  (if group-name
                                      (concat "https://" (sn-account-server sn-current-account) "/group/" group-name)
                                    (concat "https://" (sn-account-server sn-current-account) "/tag/" tag-name)))
                   tag ,tag-name
                   group ,group-name)
               `(mouse-face highlight
                            face identica-uri-face
                            uri ,uri
                            uri-in-text ,uri))
             text))
          (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (when (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
        (let ((uri (match-string-no-properties 1 source))
              (caption (match-string-no-properties 2 source)))
          (setq source caption)
          (add-text-properties
           0 (length source)
           `(mouse-face highlight
                        face identica-uri-face
                        source ,source)
           source)))

      ;; save last update time
      (setq identica-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
         `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated favorited
            in-reply-to-status-id
            in-reply-to-screen-name
            conversation-id
            user-id user-name user-screen-name user-location
            user-description
            user-profile-image-url
            user-profile-url
            user-url
            user-protected)))))

(defun identica-xmltree-to-status (xmltree)
  (mapcar #'identica-status-to-status-datum
          ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
          ;; On Emacs22, there may be blank strings
          (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
            (while statuses
              (when (consp (car statuses))
                (setq ret (cons (car statuses) ret)))
              (setq statuses (cdr statuses)))
            ret)))

(defun identica-percent-encode (str &optional coding-system)
  (if (equal (sn-account-auth-mode sn-current-account) "oauth")
      (oauth-hexify-string str)
    (when (or (null coding-system)
              (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
    (mapconcat
     (lambda (c)
       (cond
        ((identica-url-reserved-p c)
         (char-to-string c))
        ((eq c ? ) "+")
        (t (format "%%%x" c))))
     (encode-coding-string str coding-system)
     "")))

(defun identica-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun identica-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
            (found-at nil)
            (result '()))
        (while (setq found-at
                     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
                                   encoded-str cursor))
          (when (> found-at cursor)
            (push (substring encoded-str cursor found-at) result))
          (let ((number-entity (match-string-no-properties 2 encoded-str))
                (letter-entity (match-string-no-properties 3 encoded-str)))
            (cond (number-entity
                   (push
                    (char-to-string
                     (identica-ucs-to-char
                      (string-to-number number-entity))) result))
                  (letter-entity
                   (cond ((string= "gt" letter-entity) (push ">" result))
                         ((string= "lt" letter-entity) (push "<" result))
                         (t (push "?" result))))
                  (t (push "?" result)))
            (setq cursor (match-end 0))))
        (push (substring encoded-str cursor) result)
        (apply 'concat (nreverse result)))
    ""))

(defun identica-timer-action (func)
  (let ((buf (get-buffer identica-buffer)))
    (if (null buf)
        (identica-stop)
      (funcall func))))

(defun identica-update-status-if-not-blank (method-class method status &optional parameters reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (if (equal method-class "statuses")
        (identica-http-post method-class method
                            `(("status" . ,status)
                              ("source" . ,identica-source)
                              ,@(if (assoc `media parameters)
                                    `(("media" . ,(cdr (assoc `media parameters))))
                                  nil)
                              ,@(if reply-to-id
                                    `(("in_reply_to_status_id"
                                       . ,(number-to-string reply-to-id))))))
      (identica-http-post method-class method
                          `(("text" . ,status)
                            ("user" . ,parameters) ;must change this to parse parameters as list
                            ("source" . ,identica-source))))

    t))

(defvar identica-update-status-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'identica-update-status-from-edit-buffer-send)
    (define-key map (kbd "C-c C-k") 'identica-update-status-from-edit-buffer-cancel)
    map))

(define-derived-mode identica-update-status-edit-mode text-mode "Identica Status Edit"
  (use-local-map identica-update-status-edit-map))

(defvar identica-update-status-edit-method-class)
(defvar identica-update-status-edit-method)
(defvar identica-update-status-edit-parameters)
(defvar identica-update-status-edit-reply-to-id)

(defun identica-update-status-edit-in-edit-buffer (init-str msgtype method-class method parameters &optional reply-to-id)
  (let ((buf (get-buffer-create "*identica-status-update-edit*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (when (not (equal major-mode 'identica-update-status-edit-mode))
        (progn
          (identica-update-status-edit-mode)
          (when identica-soft-wrap-status
            (when (fboundp 'visual-line-mode)
              (visual-line-mode t)))
          (make-local-variable 'identica-update-status-edit-method-class)
          (make-local-variable 'identica-update-status-edit-method)
          (make-local-variable 'identica-update-status-edit-parameters)
          (make-local-variable 'identica-update-status-edit-reply-to-id)
          (if (> (length parameters) 0)
              (setq mode-line-format
                    (cons (format "%s(%s) (%%i/%s) " msgtype parameters
                                  (sn-account-textlimit sn-current-account))
                          mode-line-format))
            t (setq mode-line-format
                    (cons (format "%s (%%i/%s) " msgtype (sn-account-textlimit sn-current-account))
                          mode-line-format)))))
      (setq identica-update-status-edit-method-class method-class)
      (setq identica-update-status-edit-method method)
      (setq identica-update-status-edit-parameters parameters)
      (setq identica-update-status-edit-reply-to-id reply-to-id)
      (message identica-update-status-edit-method-class)
      (insert init-str)
      (message "Type C-c C-c to post status update (C-c C-k to cancel)."))))

(defcustom identica-minibuffer-length-prompt-style nil
  "The preferred style of counting characters in the minibuffer.
prompt; \"Down\" counts down from (sn-account-textlimit sn-current-account); \"Up\" counts
  up from 0"
  :type '(choice (const :tag "Down" nil)
                 (const :tag "Up" t))
  :group 'identica-mode)

(defun identica-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (let* ((status-len (- (buffer-size) (minibuffer-prompt-width)))
           (mes (format "%d" (if identica-minibuffer-length-prompt-style
                                 status-len
                               (- (sn-account-textlimit sn-current-account) status-len)))))
      (if (<= 23 emacs-major-version)
          (minibuffer-message mes) ; Emacs23 or later
        (minibuffer-message (concat " (" mes ")"))))))

(defun identica-setup-minibuffer ()
  (identica-show-minibuffer-length)
  (add-hook 'post-command-hook 'identica-show-minibuffer-length t t))

(defun identica-finish-minibuffer ()
  (remove-hook 'post-command-hook 'identica-show-minibuffer-length t))

(defun identica-update-status (update-input-method &optional init-str reply-to-id method-class method parameters)
  (identica-create-account)
  (when (null init-str) (setq init-str ""))
  (let ((msgtype "")
        (status init-str)
        (not-posted-p t)
        (user nil)
        (map minibuffer-local-map)
        (minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'identica-shortenurl-replace-at-point)
    (if (null method-class)
        (progn (setq msgtype "Status")
               (setq method-class "statuses")
               (setq method "update"))
      (progn (setq msgtype "Direct message")
             (setq method-class "direct_messages")
             (setq parameters (read-from-minibuffer "To user: " user nil nil nil nil t))
             (setq method "new")))
    (cond ((eq update-input-method 'minibuffer)
           (add-hook 'minibuffer-setup-hook 'identica-setup-minibuffer t)
           (add-hook 'minibuffer-exit-hook 'identica-finish-minibuffer t)
           (unwind-protect
               (while not-posted-p
                 (setq status (read-from-minibuffer (concat msgtype ": ") status nil nil nil nil t))
                 (while (< (+ (sn-account-textlimit sn-current-account) 1) (length status))
                   (setq status (read-from-minibuffer (format (concat msgtype "(%d): ")
                                                              (- (sn-account-textlimit sn-current-account) (length status)))
                                                      status nil nil nil nil t)))
                 (setq not-posted-p
                       (not (identica-update-status-if-not-blank method-class method status parameters reply-to-id))))
             (remove-hook 'minibuffer-setup-hook 'identica-setup-minibuffer)
             (remove-hook 'minibuffer-exit-hook 'identica-finish-minibuffer)))
          ((eq update-input-method 'edit-buffer)
           (identica-update-status-edit-in-edit-buffer init-str msgtype method-class method parameters reply-to-id))
          (t (error "Unknown update-input-method in identica-update-status: %S" update-input-method)))))

(defun identica-update-status-from-edit-buffer-send ()
  (interactive)
  (with-current-buffer "*identica-status-update-edit*"
    (if longlines-mode
        (longlines-encode-region (point-min) (point-max)))
    (let* ((status (buffer-substring-no-properties (point-min) (point-max)))
           (status-len (length status)))
      (if (< (sn-account-textlimit sn-current-account) status-len)
          (message (format "Beyond %s chars.  Remove %d chars."
                           (sn-account-textlimit sn-current-account)
                           (- status-len (sn-account-textlimit sn-current-account))))
        (if (identica-update-status-if-not-blank identica-update-status-edit-method-class
                                                 identica-update-status-edit-method status
                                                 identica-update-status-edit-parameters
                                                 identica-update-status-edit-reply-to-id)
            (progn
              (erase-buffer)
              (bury-buffer))
          (message "Update failed!"))))))

(defun identica-update-status-from-minibuffer (&optional init-str method-class method parameters reply-to-id)
  (interactive)
  (identica-update-status 'minibuffer init-str method-class method parameters reply-to-id))

(defun identica-update-status-from-edit-buffer (&optional init-str method-class method parameters)
  (interactive)
  (identica-update-status 'edit-buffer init-str method-class method parameters))

(defun identica-update-status-from-edit-buffer-cancel ()
  (interactive)
  (when (or (not identica-update-status-edit-confirm-cancellation)
            (yes-or-no-p
             "Really cancel editing this status message (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)))

(defun identica-update-status-from-region (beg end)
  (interactive "r")
  (when (> (- end beg) (sn-account-textlimit sn-current-account))
    (setq end (+ beg (sn-account-textlimit sn-current-account))))
  (when (< (- end beg) (sn-account-textlimit sn-current-account))
    (setq beg (+ end (sn-account-textlimit sn-current-account))))
  (identica-update-status-if-not-blank "statuses" "update" (buffer-substring beg end)))

(defun identica-update-status-with-media (attachment &optional init-str method-class method parameters reply-to-id)
  (interactive "f")
  (identica-update-status 'minibuffer nil reply-to-id nil nil `((media . ,(insert-file-contents-literally attachment)))))

(defun identica-tinyurl-unjson-google (result)
  "Gets only the URL from JSON URL tinyfying service results.

Google's shortening service, goo.gl, returns shortened URLs as a
JSON dictionary. This function retrieves only the URL value from
this dictionary, only if identica-urlshortening-service is 'google."
  (if (eq identica-urlshortening-service 'google)
      (cdr (assoc 'short_url (json-read-from-string result)))
    result))

(defun identica-ur1ca-get (api longurl)
  "Shortens url through ur1.ca free service 'as in freedom'."
  (let* ((apiurl (if (string-match "\\(http://.*\\)\\?\\(.*=\\)" api)
                  (match-string 1 api)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (datavar (match-string 2 api))
         (url-request-data (concat datavar (url-hexify-string longurl)))
         (buffer (url-retrieve-synchronously apiurl)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (prog1
          (if (string-equal identica-urlshortening-service "ur1ca")
              (if (search-forward-regexp "Your .* is: .*>\\(http://ur1.ca/[0-9A-Za-z].*\\)</a>" nil t)
                  (match-string-no-properties 1)
                (error "URL shortening service failed: %s" longurl))
            (if (search-forward-regexp "\\(http://[0-9A-Za-z/].*\\)" nil t)
                (match-string-no-properties 1)
              (error "URL shortening service failed: %s" longurl)))
        (kill-buffer buffer)))))

(defun identica-shortenurl-get (longurl)
  "Shortens url through a url shortening service."
  (let ((api (cdr (assoc identica-urlshortening-service
                         identica-urlshortening-services-map))))
    (unless api
      (error "`identica-urlshortening-service' was invalid.  try one of %s"
             (mapconcat (lambda (x)
                          (symbol-name (car x)))
                        identica-urlshortening-services-map ", ")
             "."))
    (if longurl
        (if (not (eq identica-urlshortening-service 'google))
            (identica-ur1ca-get api longurl)
          (let ((buffer (url-retrieve-synchronously (concat api longurl))))
            (with-current-buffer buffer
              (goto-char (point-min))
              (prog1
                  (identica-tinyurl-unjson-google
                   (if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
                       (match-string-no-properties 1)
                     (error "URL shortening service failed: %s" longurl)))
                (kill-buffer buffer))))
          nil))))

(defun identica-shortenurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (identica-shortenurl-get (thing-at-point 'url))))
        (when url
          (save-restriction
            (narrow-to-region (car url-bounds) (cdr url-bounds))
            (delete-region (point-min) (point-max))
            (insert url)))))))

(defun identica-expand-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url))
        (original-url (thing-at-point 'url)))
    (when url-bounds
      (message (concat "Expanding url: " original-url))
      (let ((uri (identica-expand-shorturl original-url)))
        (when uri
          (set-buffer (get-buffer identica-buffer))
          (save-restriction
            (setq buffer-read-only nil)
            (narrow-to-region (car url-bounds) (cdr url-bounds))
            (delete-region (point-min) (point-max))
            (add-text-properties 0 (length uri)
                                 `(mouse-face highlight
                                              face identica-uri-face
                                              uri ,uri
                                              uri-in-text ,uri) uri)
            (insert uri)
            (message (concat "Expanded Short URL " original-url "to Long URL: " uri))
            (setq buffer-read-only t)))))))

(defun identica-expand-shorturl (url)
  "Return the redirected URL, or the original url if not found."
  (let ((temp-buf (get-buffer-create "*HTTP headers*")))
    (set-buffer temp-buf)
    (erase-buffer)
    (goto-char 0)
    (let*
        ((url (replace-regexp-in-string "http://" "" url))
         (host (substring url 0 (string-match "/" url)))
         (file (if (string-match "/" url)
                   (substring url (string-match "/" url))
                 "/"))
         (tcp-connection (open-network-stream "Identica URLExpand"
                                              temp-buf host 80))
         (request (concat "GET http://" url " HTTP/1.1\r\n"
                          "Host:" host "\r\n"
                          "User-Agent: " (identica-user-agent) "\r\n"
                          "Authorization: None\r\n"
                          "Accept-Charset: utf-8;q=0.7,*;q=0.7\r\n\r\n")))
      (set-marker (process-mark tcp-connection) (point-min))
      (set-process-sentinel tcp-connection 'identica-http-headers-sentinel)
      (process-send-string tcp-connection request)
      (sit-for 2)
      (let ((location (identica-get-location-from-header (concat "http://" host file) tcp-connection)))
        (delete-process tcp-connection)
        (kill-buffer temp-buf)
        location))))

(defun identica-http-headers-sentinel (process string)
  "Process the results from the efine network connection."

  )

(defun identica-get-location-from-header (url process)
  "Parse HTTP header."
  (let ((buffer)
        (headers)
        (location))
    (setq buffer (get-buffer-create "*HTTP headers*"))
    (set-buffer buffer)
    (goto-char 0)
    (setq location
          (if (search-forward-regexp "^Location: \\(http://.*?\\)\r?$" nil t)
              (match-string-no-properties 1)
            url))
    (replace-regexp-in-string "\r" "" location)))

;;;
;;; Commands
;;;

(defun identica-start (&optional action)
  (interactive)
  (when (null action)
    (setq action #'identica-current-timeline))
  (if identica-timer
      nil
    (setq identica-timer
          (run-at-time "0 sec"
                       identica-timer-interval
                       #'identica-timer-action action)))
  (set 'identica-active-mode t)
  (identica-update-mode-line))

(defun identica-stop ()
  "Stop Current network activitiy (if any) and the reload-timer."
  (interactive)
  (when (get-buffer-process identica-http-buffer)
    (delete-process identica-http-buffer)
    (kill-buffer identica-http-buffer))
  (setq identica-method (sn-account-last-timeline-retrieved sn-current-account))
  (identica-set-mode-string nil)
  (and identica-timer
       (cancel-timer identica-timer))
  (setq identica-timer nil)
  (set 'identica-active-mode nil)
  (identica-update-mode-line))

(defun identica-switch-account ()
  "Update the current account and reload the default timeline."
  (interactive)
  (let ((current-account (member* sn-current-account statusnet-accounts)))
    (setq sn-current-account
          (if (cdr current-account)
              (cadr current-account)
            (car statusnet-accounts))
          identica-timeline-data nil)
    (identica-current-timeline)))

(defun identica-get-timeline (&optional server parameters)
  (setq identica-remote-server server)
  (unless parameters (setq parameters `(("count" . ,(int-to-string identica-statuses-count)))))
  (when (not (eq (sn-account-last-timeline-retrieved sn-current-account) identica-method))
    (setq identica-timeline-last-update nil
          identica-timeline-data nil))
  (let ((buf (get-buffer identica-buffer)))
    (if (not buf)
        (identica-stop)
      (progn
        (when (not identica-method)
          (setq identica-method "friends_timeline"))
        (identica-http-get (or server (sn-account-server sn-current-account))
                           (if server "none"
                             (sn-account-auth-mode sn-current-account))
                           identica-method-class identica-method parameters))))
  (identica-get-icons))

(defun identica-get-icons ()
  "Retrieve icons if icon-mode is active."
  (if identica-icon-mode
      (if (and identica-image-stack window-system)
          (let ((proc
                 (apply
                  #'start-process
                  "wget-images"
                  nil
                  "wget"
                  (format "--directory-prefix=%s" identica-tmp-dir)
                  "--no-clobber"
                  "--quiet"
                  identica-image-stack)))
            (set-process-sentinel
             proc
             (lambda (proc stat)
               (clear-image-cache)
               ))))))

(defun identica-friends-timeline ()
  (interactive)
  (setq identica-method "friends_timeline")
  (setq identica-method-class "statuses")
  (identica-get-timeline))

(defun identica-replies-timeline ()
  (interactive)
  (setq identica-method "replies")
  (setq identica-method-class "statuses")
  (identica-get-timeline))

;; (defun identica-direct-messages-timeline ()
;;   (interactive)
;;   (setq identica-method "direct_messages")
;;   (setq identica-method-class "none")
;;   (identica-get-timeline))

(defun identica-public-timeline ()
  (interactive)
  (setq identica-method "public_timeline")
  (setq identica-method-class "statuses")
  (identica-get-timeline))

(defun identica-group-timeline (&optional group)
  (interactive)
  (unless group
    (setq group (read-from-minibuffer "Group: " nil nil nil nil nil t)))
  (setq identica-method-class "statusnet/groups")
  (if (string-equal group "")
      (setq identica-method "timeline")
    (setq identica-method (concat "timeline/" group)))
  (identica-get-timeline))

(defun identica-tag-timeline (&optional tag)
  (interactive)
  (unless tag
    (setq tag (read-from-minibuffer "Tag: " nil nil nil nil nil t)))
  (setq identica-method-class "statusnet/tags")
  (if (string-equal tag "")
      (setq identica-method "timeline")
    (setq identica-method (concat "timeline/" tag)))
  (identica-get-timeline))

(defun identica-user-timeline (&optional from-user)
  "Retrieve user timeline given its username.

FROM-USER can be an empty string (\"\") meaning that you want to retrieve your own timeline.
If nil, will ask for username in minibuffer."
  (interactive)
  (unless from-user
    (setq from-user (read-from-minibuffer "User [Empty for mine]: "
                                          nil nil nil nil nil t)))
  (setq identica-method-class "statuses")
  (if (string-equal from-user "")
      (setq identica-method "user_timeline")
    (setq identica-method (concat "user_timeline/" from-user)))
  (identica-get-timeline)
  )

(defun identica-conversation-timeline ()
  (interactive)
  (let ((context-id (get-text-property (point) 'conversation-id)))
    (setq identica-method-class "statusnet")
    (setq identica-method (concat "conversation/" context-id)))
  (identica-get-timeline identica-remote-server))

(defun identica-remote-user-timeline ()
  (interactive)
  (let* ((profile (get-text-property (point) 'profile-url))
         (username (get-text-property (point) 'username))
                                        ;Strip potential trailing slashes and username references from profile url to get the server url
         (server-url (if (string-match (concat "/?\\(" username "\\)?/?$") profile)
                         (replace-match "" nil t profile)
                       profile))
         (server (if (string-match "^https?://" server-url)
                     (replace-match "" nil t server-url)
                   server-url)))
    (setq identica-method-class "statuses")
    (setq identica-method (concat "user_timeline/" username))
    (identica-get-timeline server)))

(defun identica-current-timeline (&optional count)
  "Load newer notices, with an argument load older notices, and with a numeric argument load that number of notices."
  (interactive "P")
  (if (> identica-new-dents-count 0)
      (identica-render-pending-dents)
    (identica-get-timeline
     identica-remote-server
     (if count
         (cons `("count" .
                 ,(int-to-string
                   (if (listp count) identica-statuses-count count)))
               (if (listp count)
                   `(("max_id" .
                      ,(int-to-string
                        (- (assoc-default 'id (car (last identica-timeline-data))) 1))))
                 ()))
       nil))))

(defun identica-update-status-interactive ()
  (interactive)
  (identica-update-status identica-update-status-method))

(defun identica-direct-message-interactive ()
  (interactive)
  (identica-update-status identica-update-status-method nil nil "direct_messages" "new"))

(defun identica-erase-old-statuses ()
  (interactive)
  (setq identica-timeline-data nil)
  (when (not (sn-account-last-timeline-retrieved sn-current-account))
    (setf (sn-account-last-timeline-retrieved sn-current-account) identica-method))
  (identica-http-get (sn-account-server sn-current-account) (sn-account-auth-mode sn-current-account)
                     "statuses" (sn-account-last-timeline-retrieved sn-current-account)))

(defun identica-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (when uri (browse-url uri))))

(defun identica-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (get-text-property (point) 'id))
        (uri (get-text-property (point) 'uri))
        (group (get-text-property (point) 'group))
        (tag (get-text-property (point) 'tag)))
    (if group (identica-group-timeline group)
      (if tag (identica-tag-timeline tag)
        (if uri (browse-url uri)
          (if username
              (identica-update-status identica-update-status-method
                                      (concat "@" username " ") id)))))))

(defun identica-next-link nil
  (interactive)
  (goto-char (next-single-property-change (point) 'uri))
  (when (not (get-text-property (point) 'uri))
    (goto-char (next-single-property-change (point) 'uri))))

(defun identica-prev-link nil
  (interactive)
  (goto-char (previous-single-property-change (point) 'uri))
  (when (not (get-text-property (point) 'uri))
    (goto-char (previous-single-property-change (point) 'uri))))

(defun identica-follow (&optional remove)
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (method (if remove "destroy" "create"))
        (message (if remove "unfollowing" "following")))
    (unless username
      (setq username (read-from-minibuffer "user: ")))
    (if (> (length username) 0)
        (when (y-or-n-p (format "%s %s? " message username))
          (identica-http-post (format "friendships/%s" method) username)
          (message (format "Now %s %s" message username)))
      (message "No user selected"))))

(defun identica-unfollow ()
  (interactive)
  (identica-follow t))

(defun identica-group-join (&optional leaving)
  "Simple functions to join/leave a group we are visiting."
  (setq identica-method-class "statusnet/groups")
  (string-match "\\([^\\]*\\)\\(/.*\\)" identica-method)
  (let ((group-method (replace-match
                       (if leaving "leave"
                         "join") nil nil identica-method 1)))
    (identica-http-post identica-method-class group-method nil)))

(defun identica-group-leave ()
  (identica-group-join t))

(defun identica-favorite ()
  (interactive)
  (when (y-or-n-p "Do you want to favor this notice? ")
    (let ((id (get-text-property (point) 'id)))
      (identica-http-post "favorites/create" (number-to-string id))
      (message "Notice saved as favorite"))))

(defun identica-repeat ()
  (interactive)
  (when (y-or-n-p "Do you want to repeat this notice? ")
    (let ((id (get-text-property (point) 'id)))
      (identica-http-post "statuses/retweet" (number-to-string id))
      (message "Notice repeated"))))

(defun identica-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (when uri (browse-url uri))))

(defun identica-redent ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (get-text-property (point) 'id))
        (text (replace-regexp-in-string "!\\(\\b\\)" "#\\1" (get-text-property (point) 'text))))
    (when username
      (identica-update-status identica-update-status-method
                              (concat identica-redent-format " @" username ": " text) id))))

(defun identica-reply-to-user (all)
  "Open a minibuffer initialized to type a reply to the notice at point.
With no argument, populate with the username of the author of the notice.
With an argument, populate with the usernames of the author and any usernames mentioned in the notice."
  (interactive "P")
  (let ((username (get-text-property (point) 'username))
        (notice-text (get-text-property (point) 'text))
        (id (get-text-property (point) 'id))
        (usernames nil)
        (usernames-string ""))
    (when all
      (setq usernames
            (mapcar (lambda (string)
                      (when (and (char-equal (aref string 0) ?@)
                                 (memq-face identica-uri-face
                                            (get-text-property 2 'face string)))
                        (concat string " ")))
                    (split-string notice-text))))
    (when username (setq usernames (cons (concat "@" username " ") usernames)))
    (setq usernames (delete-dups usernames))
    (setq usernames (delete (concat "@" (sn-account-username sn-current-account) " ") usernames))
    (setq usernames-string (apply 'concat usernames))
    (identica-update-status identica-update-status-method usernames-string id)))

(defun identica-reply-to-all ()
  (interactive)
  (identica-reply-to-user t))

(defun identica-get-password ()
  (or (sn-account-password sn-current-account)
      (setf (sn-account-password sn-current-account) (read-passwd "password: "))))

(defun identica-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (identica-get-next-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (progn (goto-char (buffer-end 1)) (message "End of status.")))))

(defun identica-toggle-highlight (&optional arg)
  "Toggle the highlighting of entry at 'point'.
With no arg or prefix, toggle the highlighting of the entry at 'point'.
With arg (or prefix, if interactive), highlight the current entry and
un-highlight all other entries."
  (interactive "P")
  (let ((id (get-text-property (point) 'id)))
    (setq identica-highlighted-entries
          (if arg (list id)
            (if (memq id identica-highlighted-entries)
                (delq id identica-highlighted-entries)
              (cons id identica-highlighted-entries)))))
  (identica-render-timeline))

(defun memq-face (face property)
  "Check whether FACE is present in PROPERTY."
  (if (listp property)
      (memq face property)
    (eq property face)))

(defun identica-get-next-username-face-pos (pos &optional object)
  "Returns the position of the next username after POS, or nil when end of string or buffer is reached."
  (interactive "P")
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (memq-face identica-username-face prop)))
        (setq pos (next-single-property-change pos 'face object))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face object)))
      pos)))

(defun identica-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (identica-get-previous-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (message "Start of status."))))

(defun identica-get-previous-username-face-pos (pos &optional object)
  "Returns the position of the previous username before POS, or nil when start of string or buffer is reached."
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (memq-face identica-username-face prop)))
        (setq pos (previous-single-property-change pos 'face object))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face object)))
      pos)))

(defun identica-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (identica-get-username-at-pos (point)))
        (pos (identica-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (identica-get-username-at-pos pos) user-name)))
      (setq pos (identica-get-next-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "End of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun identica-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (identica-get-username-at-pos (point)))
        (pos (identica-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (identica-get-username-at-pos pos) user-name)))
      (setq pos (identica-get-previous-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "Start of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun identica-get-username-at-pos (pos)
  (let ((start-pos pos)
        (end-pos))
    (catch 'not-found
      (while (memq-face identica-username-face (get-text-property start-pos 'face))
        (setq start-pos (1- start-pos))
        (when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun assoc-workaround (tag array)
  "Workaround odd semi-associative array returned by url-http."
  (or (assoc tag array)
      (and (equal tag (car array))
           (cadr array))))

(defun identica-get-status-url (id)
  "Generate status URL."
  (format "https://%s/notice/%s" (sn-account-server sn-current-account) id))

(defun identica-get-context-url (id)
  "Generate status URL."
  (format "https://%s/conversation/%s" (sn-account-server sn-current-account) id))

(defun identica-retrieve-configuration ()
  "Retrieve the configuration for the current statusnet server."
  (identica-http-get (sn-account-server sn-current-account) (sn-account-auth-mode sn-current-account)
                     "statusnet" "config" nil 'identica-http-get-config-sentinel))

(defun identica-http-get-config-sentinel
  (&optional status method-class method parameters success-message)
  "Process configuration page retrieved from statusnet server."
  (let ((error-object (assoc-workaround :error status)))
    (unless error-object
      (let* ((body (identica-get-response-body))
             (site (xml-get-children (car body) 'site))
             (textlimit (xml-get-children (car site) 'textlimit))
             (textlimit-value (caddar textlimit)))
        (when (> (string-to-number textlimit-value) 0)
          (setf (sn-account-textlimit sn-current-account) (string-to-number textlimit-value))))))
  (identica-start))

(defun identica-get-config-url ()
  "Generate configuration URL."
  (format "http://%s/api/statusnet/config.xml" (sn-account-server sn-current-account)))

;; Icons
;;; ACTIVE/INACTIVE
(defconst identica-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char * statusnet_xpm[] = {
\"16 16 14 1\",
\"  c None\",
\".	c #8F0000\",
\"+	c #AB4040\",
\"@	c #D59F9F\",
\"#	c #E3BFBF\",
\"$	c #CE8F8F\",
\"%	c #C78080\",
\"&	c #FFFFFF\",
\"*	c #B96060\",
\"=	c #DCAFAF\",
\"-	c #C07070\",
\";	c #F1DFDF\",
\">	c #961010\",
\",	c #9D2020\",
\"    .......     \",
\"   .........    \",
\"  ...........   \",
\" ....+@#$+....  \",
\"....%&&&&&*.... \",
\"...+&&&&&&&+... \",
\"...=&&&&&&&$... \",
\"...#&&&&&&&#... \",
\"...=&&&&&&&@... \",
\"...*&&&&&&&-... \",
\"....@&&&&&&=... \",
\" ....-#&#$;&>.. \",
\" ..........,>.. \",
\"  ............. \",
\"    ............\",
\"       .      ..\"};")))

(defconst identica-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char * statusnet_off_xpm[] = {
\"16 16 13 1\",
\"  g None\",
\".	g #5B5B5B\",
\"+	g #8D8D8D\",
\"@	g #D6D6D6\",
\"#	g #EFEFEF\",
\"$	g #C9C9C9\",
\"%	g #BEBEBE\",
\"&	g #FFFFFF\",
\"*	g #A5A5A5\",
\"=	g #E3E3E3\",
\"-	g #B2B2B2\",
\";	g #676767\",
\">	g #747474\",
\"    .......     \",
\"   .........    \",
\"  ...........   \",
\" ....+@#$+....  \",
\"....%&&&&&*.... \",
\"...+&&&&&&&+... \",
\"...=&&&&&&&$... \",
\"...#&&&&&&&#... \",
\"...=&&&&&&&@... \",
\"...*&&&&&&&-... \",
\"....@&&&&&&=... \",
\" ....-#&#$&&;.. \",
\" ..........>;.. \",
\"  ............. \",
\"    ............\",
\"       .      ..\"};")))

(let ((props
       (when (display-mouse-p)
         `(local-map
           ,(purecopy (make-mode-line-mouse-map
                       'mouse-2 #'identica-toggle-activate-buffer))
           help-echo "mouse-2 toggles automatic updates"))))
  (defconst identica-modeline-active
    (if identica-active-indicator-image
        (apply 'propertize " "
               `(display ,identica-active-indicator-image ,@props))
      " "))
  (defconst identica-modeline-inactive
    (if identica-inactive-indicator-image
        (apply 'propertize "INACTIVE"
               `(display ,identica-inactive-indicator-image ,@props))
      "INACTIVE")))

(defun identica-toggle-activate-buffer ()
  (interactive)
  (setq identica-active-mode (not identica-active-mode))
  (if (not identica-active-mode)
      (identica-stop)
    (identica-start)))

(defun identica-mode-line-buffer-identification ()
  (if identica-active-mode
      identica-modeline-active
    identica-modeline-inactive))

(defun identica-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

;;;###autoload
(defun identica ()
  "Start identica-mode."
  (interactive)
  (identica-mode))

(provide 'identica-mode)
(add-hook 'identica-load-hook 'identica-autoload-oauth)
(run-hooks 'identica-load-hook)

;;; identica-mode.el ends here
