;;; geiser-log.el -- logging utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2009, 2010, 2012, 2019, 2021, 2022 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Sat Feb 07, 2009 12:07

;;; Commentary:

;; Some utilities for maintaining a simple log buffer, mainly for
;; debugging purposes.


;;; Code:

(require 'geiser-custom)
(require 'geiser-popup)
(require 'geiser-base)

(require 'comint)


;;; Customization:

(geiser-custom--defcustom geiser-log-autoscroll-buffer-p nil
  "Set this so than the buffer *Geiser Messages* always shows the last message"
  :group 'geiser
  :type 'boolean)

(defvar geiser-log--buffer-name "*Geiser Messages*"
  "Name of the Geiser log buffer.")

(defvar geiser-log--max-buffer-size 320000
  "Maximum size of the Geiser messages log.")

(defvar geiser-log--max-message-size 20480
  "Maximum size of individual Geiser log messages.")

(define-obsolete-variable-alias
  'geiser-log-verbose-p 'geiser-log-verbose "0.26.2")

(defvar geiser-log-verbose nil
  "Log purely informational messages.")

(define-obsolete-variable-alias
  'geiser-log-verbose-debug-p 'geiser-log-verbose-debug "0.26.2")

(defvar geiser-log-verbose-debug nil
  "Log very verbose informational messages. Useful only for debugging.")


(defvar geiser-log--inhibit nil
  "Set this to t to inhibit all log messages")


;;; Log buffer and mode:

(defvar geiser-messages-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'geiser-log-clear)
    (define-key map "Q" 'geiser-log--deactivate)
    map))

(define-derived-mode geiser-messages-mode fundamental-mode "Geiser Messages"
  "Simple mode for Geiser log messages buffer."
  (buffer-disable-undo)
  (add-hook 'after-change-functions
            (lambda (b _e _len)
              (let ((inhibit-read-only t))
                (when (> b geiser-log--max-buffer-size)
                  (delete-region (point-min) b))))
            nil t)
  ;; Maybe this feature would better be implemented as a revert-buffer function?
  (add-hook 'after-change-functions
            (lambda (_b _e _len)
              (when geiser-log-autoscroll-buffer-p
                (let ((my-window (get-buffer-window (geiser-log--buffer) t)))
                  (when (window-live-p my-window)
                    (set-window-point my-window (point))))))
            nil t)
  (setq buffer-read-only t))

(geiser-popup--define log geiser-log--buffer-name geiser-messages-mode)


;;; Logging functions:

(defun geiser-log--msg (type &rest args)
  (unless geiser-log--inhibit
    (geiser-log--with-buffer
      (goto-char (point-max))
      (insert (geiser--shorten-str (format "\n%s: %s\n" type
                                           (apply 'format args))
                                   geiser-log--max-message-size)))))

(defsubst geiser-log--warn (&rest args)
  (apply 'geiser-log--msg 'WARNING args))

(defsubst geiser-log--error (&rest args)
  (apply 'geiser-log--msg 'ERROR args))

(defsubst geiser-log--info (&rest args)
  (when geiser-log-verbose
    (apply 'geiser-log--msg 'INFO args) ""))

(defsubst geiser-log--debug (&rest args)
  (when geiser-log-verbose-debug
    (apply 'geiser-log--msg 'DEBUG args) ""))


;;; User commands:

(defun geiser-show-logs (&optional arg)
  "Show Geiser log messages.

With prefix, activates all logging levels."
  (interactive "P")
  (setq geiser-log-verbose t)
  (when arg
    (setq geiser-log-verbose-debug t))
  (geiser-log--pop-to-buffer))

(defun geiser-log-clear ()
  "Clean all logs."
  (interactive)
  (geiser-log--with-buffer (delete-region (point-min) (point-max))))

(defun geiser-log-toggle-verbose ()
  "Toggle verbose logs"
  (interactive)
  (setq geiser-log-verbose (not geiser-log-verbose))
  (message "Geiser verbose logs %s"
           (if geiser-log-verbose "enabled" "disabled")))

(defun geiser-log--deactivate ()
  (interactive)
  (setq geiser-log-verbose nil)
  (when (eq (current-buffer) (geiser-log--buffer)) (View-quit)))


(provide 'geiser-log)
