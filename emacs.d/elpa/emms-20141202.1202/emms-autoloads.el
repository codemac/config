;;; emms-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "emms-cache" "emms-cache.el" (21632 56505 868191
;;;;;;  827000))
;;; Generated autoloads from emms-cache.el

(autoload 'emms-cache-enable "emms-cache" "\
Enable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-disable "emms-cache" "\
Disable caching of Emms track data.

\(fn)" t nil)

(autoload 'emms-cache-toggle "emms-cache" "\
Toggle caching of Emms track data.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-lyrics" "emms-lyrics.el" (21632 56505
;;;;;;  474860 57000))
;;; Generated autoloads from emms-lyrics.el

(autoload 'emms-lyrics-enable "emms-lyrics" "\
Enable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-disable "emms-lyrics" "\
Disable displaying emms lyrics.

\(fn)" t nil)

(autoload 'emms-lyrics-toggle "emms-lyrics" "\
Toggle displaying emms lyrics.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-mode-line" "emms-mode-line.el" (21632
;;;;;;  56505 938191 548000))
;;; Generated autoloads from emms-mode-line.el

(autoload 'emms-mode-line-enable "emms-mode-line" "\
Turn on `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-disable "emms-mode-line" "\
Turn off `emms-mode-line'.

\(fn)" t nil)

(autoload 'emms-mode-line-toggle "emms-mode-line" "\
Toggle `emms-mode-line'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-player-mpd" "emms-player-mpd.el" (21632
;;;;;;  56505 524859 858000))
;;; Generated autoloads from emms-player-mpd.el

(autoload 'emms-player-mpd-clear "emms-player-mpd" "\
Clear the MusicPD playlist.

\(fn)" t nil)

(autoload 'emms-player-mpd-connect "emms-player-mpd" "\
Connect to MusicPD and retrieve its current playlist.

Afterward, the status of MusicPD will be tracked.

This also has the effect of changing the current EMMS playlist to
be the same as the current MusicPD playlist.  Thus, this
function is useful to call if the contents of the EMMS playlist
buffer get out-of-sync for some reason.

\(fn)" t nil)

(autoload 'emms-player-mpd-show "emms-player-mpd" "\
Describe the current EMMS track in the minibuffer.

If INSERTP is non-nil, insert the description into the current
buffer instead.

If CALLBACK is a function, call it with the current buffer and
description as arguments instead of displaying the description or
inserting it.

This function uses `emms-show-format' to format the current track.
It differs from `emms-show' in that it asks MusicPD for the current track,
rather than EMMS.

\(fn &optional INSERTP CALLBACK)" t nil)

;;;***

;;;### (autoloads nil "emms-playing-time" "emms-playing-time.el"
;;;;;;  (21632 56505 404860 335000))
;;; Generated autoloads from emms-playing-time.el

(autoload 'emms-playing-time-enable-display "emms-playing-time" "\
Display playing time on mode line.

\(fn)" t nil)

(autoload 'emms-playing-time-disable-display "emms-playing-time" "\
Remove playing time from mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-playlist-limit" "emms-playlist-limit.el"
;;;;;;  (21632 56505 341527 252000))
;;; Generated autoloads from emms-playlist-limit.el

(autoload 'emms-playlist-limit-enable "emms-playlist-limit" "\
Turn on emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-disable "emms-playlist-limit" "\
Turn off emms playlist limit.

\(fn)" t nil)

(autoload 'emms-playlist-limit-toggle "emms-playlist-limit" "\
Toggle emms playlist limit.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-playlist-mode" "emms-playlist-mode.el"
;;;;;;  (21632 56505 651526 21000))
;;; Generated autoloads from emms-playlist-mode.el

(autoload 'emms-playlist-mode "emms-playlist-mode" "\
A major mode for Emms playlists.
\\{emms-playlist-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-score" "emms-score.el" (21632 56505 418193
;;;;;;  615000))
;;; Generated autoloads from emms-score.el

(autoload 'emms-score-enable "emms-score" "\
Turn on emms-score.

\(fn)" t nil)

(autoload 'emms-score-disable "emms-score" "\
Turn off emms-score.

\(fn)" t nil)

(autoload 'emms-score-toggle "emms-score" "\
Toggle emms-score.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-setup" "emms-setup.el" (21632 56505 904858
;;;;;;  347000))
;;; Generated autoloads from emms-setup.el

(autoload 'emms-minimalistic "emms-setup" "\
An Emms setup script.
Invisible playlists and all the basics for playing media.

\(fn)" nil nil)

(autoload 'emms-standard "emms-setup" "\
An Emms setup script.
Everything included in the `emms-minimalistic' setup, the Emms
interactive playlist mode, reading information from tagged
audio files, and a metadata cache.

\(fn)" nil nil)

(autoload 'emms-all "emms-setup" "\
An Emms setup script.
Everything included in the `emms-standard' setup and adds all the
stable features which come with the Emms distribution.

\(fn)" nil nil)

(autoload 'emms-devel "emms-setup" "\
An Emms setup script.
Everything included in the `emms-all' setup and adds all the
features which come with the Emms distribution regardless of if
they are considered stable or not.  Use this if you like living
on the edge.

\(fn)" nil nil)

(autoload 'emms-default-players "emms-setup" "\
Set `emms-player-list' to `emms-setup-default-player-list'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "emms-source-file" "emms-source-file.el" (21632
;;;;;;  56505 438193 536000))
;;; Generated autoloads from emms-source-file.el
 (autoload 'emms-play-file "emms-source-file" nil t)
 (autoload 'emms-add-file "emms-source-file" nil t)
 (autoload 'emms-play-directory "emms-source-file" nil t)
 (autoload 'emms-add-directory "emms-source-file" nil t)
 (autoload 'emms-play-directory-tree "emms-source-file" nil t)
 (autoload 'emms-add-directory-tree "emms-source-file" nil t)
 (autoload 'emms-play-find "emms-source-file" nil t)
 (autoload 'emms-add-find "emms-source-file" nil t)
 (autoload 'emms-play-dired "emms-source-file" nil t)
 (autoload 'emms-add-dired "emms-source-file" nil t)

(autoload 'emms-source-file-directory-tree "emms-source-file" "\
Return a list of all files under DIR that match REGEX.
This function uses `emms-source-file-directory-tree-function'.

\(fn DIR REGEX)" nil nil)

(autoload 'emms-source-file-regex "emms-source-file" "\
Return a regexp that matches everything any player (that supports
files) can play.

\(fn)" nil nil)

(autoload 'emms-locate "emms-source-file" "\
Search for REGEXP and display the results in a locate buffer

\(fn REGEXP)" t nil)
 (autoload 'emms-play-url "emms-source-file" nil t)
 (autoload 'emms-add-url "emms-source-file" nil t)
 (autoload 'emms-play-streamlist "emms-source-file" nil t)
 (autoload 'emms-add-streamlist "emms-source-file" nil t)

;;;***

;;;### (autoloads nil "emms-source-playlist" "emms-source-playlist.el"
;;;;;;  (21632 56505 324860 652000))
;;; Generated autoloads from emms-source-playlist.el
 (autoload 'emms-play-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-native-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-m3u-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-add-pls-playlist "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-file "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory
          "emms-source-playlist" nil t)
 (autoload 'emms-play-playlist-directory-tree
          "emms-source-playlist" nil t)
 (autoload 'emms-add-playlist-directory-tree
          "emms-source-file" nil t)

;;;***

;;;### (autoloads nil "emms-streams" "emms-streams.el" (21632 56505
;;;;;;  684859 223000))
;;; Generated autoloads from emms-streams.el

(autoload 'emms-streams "emms-streams" "\
Opens the EMMS Streams interface.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-volume" "emms-volume.el" (21632 56505
;;;;;;  298194 91000))
;;; Generated autoloads from emms-volume.el

(autoload 'emms-volume-raise "emms-volume" "\
Raise the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-lower "emms-volume" "\
Lower the speaker volume.

\(fn)" t nil)

(autoload 'emms-volume-mode-plus "emms-volume" "\
Raise volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

(autoload 'emms-volume-mode-minus "emms-volume" "\
Lower volume and enable or extend the `emms-volume-minor-mode' timeout.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "emms-volume-amixer" "emms-volume-amixer.el"
;;;;;;  (21632 56505 378193 774000))
;;; Generated autoloads from emms-volume-amixer.el

(autoload 'emms-volume-amixer-change "emms-volume-amixer" "\
Change amixer master volume by AMOUNT.

\(fn AMOUNT)" nil nil)

;;;***

;;;### (autoloads nil "tq" "tq.el" (21632 56505 768192 224000))
;;; Generated autoloads from tq.el

(autoload 'tq-create "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

\(fn PROCESS)" nil nil)

;;;***

;;;### (autoloads nil nil ("emms-bookmarks.el" "emms-browser.el"
;;;;;;  "emms-compat.el" "emms-cue.el" "emms-history.el" "emms-i18n.el"
;;;;;;  "emms-info-libtag.el" "emms-info-metaflac.el" "emms-info-mp3info.el"
;;;;;;  "emms-info-ogginfo.el" "emms-info.el" "emms-last-played.el"
;;;;;;  "emms-librefm-scrobbler.el" "emms-librefm-stream.el" "emms-maint.el"
;;;;;;  "emms-mark.el" "emms-metaplaylist-mode.el" "emms-mode-line-icon.el"
;;;;;;  "emms-pkg.el" "emms-player-mpg321-remote.el" "emms-player-mplayer.el"
;;;;;;  "emms-player-simple.el" "emms-player-vlc.el" "emms-player-xine.el"
;;;;;;  "emms-playlist-sort.el" "emms-stream-info.el" "emms-tag-editor.el"
;;;;;;  "emms-url.el" "emms.el" "jack.el" "later-do.el") (21632 56506
;;;;;;  34957 937000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; emms-autoloads.el ends here
