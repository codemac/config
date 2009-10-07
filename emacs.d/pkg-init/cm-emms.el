;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMMS
;;
(require 'emms-setup)
(require 'emms-mode-line-icon)
(require 'emms-info-libtag)

(emms-devel)
(emms-default-players)
(setq emms-info-asynchronosly t)
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")
(setq emms-player-mpg321-parameters '("-o" "alsa"))
(setq emms-source-list '((emms-directory-tree "~/muse/")))

(setq emms-player-mplayer-parameters (list "-slave" "-nortc" "-quiet" "-really-quiet"))
(emms-player-set emms-player-mplayer 'regex
                 "\\.ogg\\|\\.mp3\\|\\.wav\\|\\.mpg\\|\\.mpeg\\|\\.wmv\\|\\.wma\\|\\.mov\\|\\.avi\\|\\.divx\\|\\.ogm\\|\\.asf\\|\\.mkv\\|http://\\|mms://\\|\\.rm\\|\\.rmvb\\|\\.mp4\\|\\.flac\\|\\.vob\\|\\.m4a\\|\\.ape\\|\\.mpc")


(setq emms-mode-line-icon-before-format "["
      emms-mode-line-format " %s]"
      emms-mode-line-icon-color "lightgrey")

;; Libtag support


(add-to-list 'emms-info-functions 'emms-info-libtag)

;;; Stolen and adapted from TWB
;(defun my-emms-info-track-description (track)
;  "Return a description of the current track."
;  (if (and (emms-track-get track 'info-artist)
;           (emms-track-get track 'info-title))
;      (let ((pmin (emms-track-get track 'info-playing-time-min))
;            (psec (emms-track-get track 'info-playing-time-sec))
;            (ptot (emms-track-get track 'info-playing-time))
;            (art  (emms-track-get track 'info-artist))
;            (tit  (emms-track-get track 'info-title)))
;        (cond ((and pmin psec) (format "%s - %s [%02d:%02d]" art tit pmin psec))
;              (ptot (format  "%s - %s [%02d:%02d]" art tit (/ ptot 60) (% ptot 60)))
;              (t (emms-track-simple-description track))))))

;(setq emms-track-description-function 'my-emms-info-track-description)

;; last.fm
(setq emms-lastfm-username cm-lastfm-username
      emms-lastfm-password cm-lastfm-password)
(emms-lastfm-enable)

(provide 'cm-emms)