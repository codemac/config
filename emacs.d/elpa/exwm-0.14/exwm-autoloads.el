;;; exwm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "exwm" "exwm.el" (22888 59612 701726 182000))
;;; Generated autoloads from exwm.el

(autoload 'exwm-reset "exwm" "\
Reset window to standard state: non-fullscreen, line-mode.

\(fn)" t nil)

(autoload 'exwm-restart "exwm" "\
Restart EXWM.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "exwm-floating" "exwm-floating.el" (22888 59612
;;;;;;  681726 394000))
;;; Generated autoloads from exwm-floating.el

(autoload 'exwm-floating-toggle-floating "exwm-floating" "\
Toggle the current window between floating and non-floating states.

\(fn)" t nil)

(autoload 'exwm-floating-hide "exwm-floating" "\
Hide the current floating X window (which would show again when selected).

\(fn)" t nil)

;;;***

;;;### (autoloads nil "exwm-input" "exwm-input.el" (22888 59612 661726
;;;;;;  606000))
;;; Generated autoloads from exwm-input.el

(autoload 'exwm-input-grab-keyboard "exwm-input" "\
Switch to line-mode.

\(fn &optional ID)" t nil)

(autoload 'exwm-input-release-keyboard "exwm-input" "\
Switch to char-mode.

\(fn &optional ID)" t nil)

(autoload 'exwm-input-toggle-keyboard "exwm-input" "\
Toggle between 'line-mode' and 'char-mode'.

\(fn &optional ID)" t nil)

(autoload 'exwm-input-send-next-key "exwm-input" "\
Send next key to client window.

\(fn TIMES)" t nil)

(autoload 'exwm-input-send-simulation-key "exwm-input" "\
Fake a key event according to last input key sequence.

\(fn TIMES)" t nil)

;;;***

;;;### (autoloads nil "exwm-layout" "exwm-layout.el" (22888 59612
;;;;;;  709726 96000))
;;; Generated autoloads from exwm-layout.el

(autoload 'exwm-layout-set-fullscreen "exwm-layout" "\
Make window ID fullscreen.

\(fn &optional ID)" t nil)

(autoload 'exwm-layout-unset-fullscreen "exwm-layout" "\
Restore window from fullscreen state.

\(fn &optional ID)" t nil)

(autoload 'exwm-layout-toggle-fullscreen "exwm-layout" "\
Toggle fullscreen mode.

\(fn &optional ID)" t nil)

(autoload 'exwm-layout-enlarge-window "exwm-layout" "\
Make the selected window DELTA pixels taller.

If no argument is given, make the selected window one pixel taller.  If the
optional argument HORIZONTAL is non-nil, make selected window DELTA pixels
wider.  If DELTA is negative, shrink selected window by -DELTA pixels.

Normal hints are checked and regarded if the selected window is displaying an
`exwm-mode' buffer.  However, this may violate the normal hints set on other X
windows.

\(fn DELTA &optional HORIZONTAL)" t nil)

(autoload 'exwm-layout-enlarge-window-horizontally "exwm-layout" "\
Make the selected window DELTA pixels wider.

See also `exwm-layout-enlarge-window'.

\(fn DELTA)" t nil)

(autoload 'exwm-layout-shrink-window "exwm-layout" "\
Make the selected window DELTA pixels lower.

See also `exwm-layout-enlarge-window'.

\(fn DELTA)" t nil)

(autoload 'exwm-layout-shrink-window-horizontally "exwm-layout" "\
Make the selected window DELTA pixels narrower.

See also `exwm-layout-enlarge-window'.

\(fn DELTA)" t nil)

(autoload 'exwm-layout-hide-mode-line "exwm-layout" "\
Hide mode-line.

\(fn)" t nil)

(autoload 'exwm-layout-show-mode-line "exwm-layout" "\
Show mode-line.

\(fn)" t nil)

(autoload 'exwm-layout-toggle-mode-line "exwm-layout" "\
Toggle the display of mode-line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "exwm-workspace" "exwm-workspace.el" (22888
;;;;;;  59612 697726 224000))
;;; Generated autoloads from exwm-workspace.el

(autoload 'exwm-workspace--get-geometry "exwm-workspace" "\
Return the geometry of frame FRAME.

\(fn FRAME)" nil nil)

(autoload 'exwm-workspace--current-width "exwm-workspace" "\
Return the width of current workspace.

\(fn)" nil nil)

(autoload 'exwm-workspace--current-height "exwm-workspace" "\
Return the height of current workspace.

\(fn)" nil nil)

(autoload 'exwm-workspace--minibuffer-own-frame-p "exwm-workspace" "\
Reports whether the minibuffer is displayed in its own frame.

\(fn)" nil nil)

(autoload 'exwm-workspace-switch "exwm-workspace" "\
Switch to workspace INDEX.  Query for FRAME-OR-INDEX if it's not specified.

The optional FORCE option is for internal use only.

\(fn FRAME-OR-INDEX &optional FORCE)" t nil)

(autoload 'exwm-workspace-switch-create "exwm-workspace" "\
Switch to workspace FRAME-OR-INDEX, creating it if it does not exist yet.

\(fn FRAME-OR-INDEX)" t nil)

(autoload 'exwm-workspace-swap "exwm-workspace" "\
Interchange position of WORKSPACE1 with that of WORKSPACE2.

\(fn WORKSPACE1 WORKSPACE2)" t nil)

(autoload 'exwm-workspace-move "exwm-workspace" "\
Move WORKSPACE to the NTH position.
When called interactively, prompt for a workspace and move current one just
before it.

\(fn WORKSPACE NTH)" t nil)

(autoload 'exwm-workspace-add "exwm-workspace" "\
Add a workspace as the INDEX-th workspace, or the last one if INDEX is nil.

INDEX must not exceed the current number of workspaces.

\(fn &optional INDEX)" t nil)

(autoload 'exwm-workspace-delete "exwm-workspace" "\
Delete the workspace FRAME-OR-INDEX.

\(fn &optional FRAME-OR-INDEX)" t nil)

(autoload 'exwm-workspace-move-window "exwm-workspace" "\
Move window ID to workspace FRAME-OR-INDEX.

\(fn FRAME-OR-INDEX &optional ID)" t nil)

(autoload 'exwm-workspace-switch-to-buffer "exwm-workspace" "\
Make the current Emacs window display another buffer.

\(fn BUFFER-OR-NAME)" t nil)

(autoload 'exwm-workspace-attach-minibuffer "exwm-workspace" "\
Attach the minibuffer so that it always shows.

\(fn)" t nil)

(autoload 'exwm-workspace-detach-minibuffer "exwm-workspace" "\
Detach the minibuffer so that it automatically hides.

\(fn)" t nil)

(autoload 'exwm-workspace-toggle-minibuffer "exwm-workspace" "\
Attach the minibuffer if it's detached, or detach it if it's attached.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("exwm-cm.el" "exwm-config.el" "exwm-core.el"
;;;;;;  "exwm-manage.el" "exwm-pkg.el" "exwm-randr.el" "exwm-systemtray.el")
;;;;;;  (22888 59612 693726 267000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; exwm-autoloads.el ends here
