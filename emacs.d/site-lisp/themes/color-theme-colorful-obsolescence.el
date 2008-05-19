;;------------------------------------------------------------------------------
;; Index for color-theme-colorful-obsolescence by Scott Jaderholm
;;------------------------------------------------------------------------------

;; Overview

;; Latest: http://jaderholm.com/configs/color-theme-colorful-obsolescence.el
;; Screenshot: http://jaderholm.com/photos/screens/colorful-obsolescence.png
;; License: GNU Lesser General Public License 2.1

;; Main features
;; 1. Looks the same in console and graphical
;; 2. Looks good if window is transparent
;; 3. Has org-mode faces

;; For console you'll need the following colors:
;;    Black 0,0,0 #000000
;;    Black Bold 85,85,85 #555555 
;;    Red 238,60,60 #ee3c3c 
;;    Red Bold 255,85,85 #ff5555 
;;    Green 0,187,0 #00bb00 
;;    Green Bold 85,255,85 #55ff55 
;;    Yellow* 255,217,85 #ffd955 
;;    Yellow Bold 255,255,85 #ffff55 
;;    Blue 85,85,255 #5555ff 
;;    Blue Bold 0,142,209 #008ed1 
;;    Magenta 150,81,204 #9651cc 
;;    Magenta Bold 255,85,255 #ff55ff 
;;    Cyan* 0,139,139 #008b8b 
;;    Cyan Bold* 0,255,255 #00ffff 
;;    White 187,187,187 #bbbbbb 
;;    White Bold 255,255,255 #ffffff 
;;    Default Foreground 187,187,187 #bbbbbb 
;;    Default Bold Foreground 255,255,255 #ffffff 
;;    Default Background 0,0,0 #000000 
;;    Default Bold Background 85,85,85 #555555 
;;    Cursor Text 0,0,0 #000000 
;;    Cursor Colour 255,255,255 #ffffff

(defun color-theme-colorful-obsolescence ()
  "Color theme Colorful Obsolescence by Scott Jaderholm, created 2007-01-05."
  (interactive)
  (let ((color-theme-is-cumulative nil))

;;-----------------------------------------------------------------------------
;; Window system
;;-----------------------------------------------------------------------------
(if window-system
    (color-theme-install
     '(color-theme-colorful-obsolescence

;;-----------------------------------------------------------------------------
;; General
;;-----------------------------------------------------------------------------
       ((background-color . "black")
	(background-mode . dark)
	(background-toolbar-color . "black")
	(border-color . "black")
	(bottom-toolbar-shadow-color . "black")
	(cursor-color	. "white")
	(foreground-color . "#bbb")
	(top-toolbar-shadow-color . "#111"))
       (default ((t (nil))))
       (button ((t (:bold t))))
       (fringe ((t (:background "#000" :foreground "#444"))))

;;-----------------------------------------------------------------------------
;; Main colors
;;-----------------------------------------------------------------------------
;; Strongest color (bright green)
     (font-lock-function-name-face ((t (:foreground "#55ff55")))) 
     (org-scheduled-today ((t (:inherit font-lock-function-name-face))))
     (org-special-keyword ((t (:inherit font-lock-function-name-face))))
     (org-tag ((t (:inherit font-lock-function-name-face))))

;; Pleasure-to-read color (light blue)
     (font-lock-comment-face ((t (:foreground "#008ed1"))))
     (font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
     (org-level-3 ((t (:inherit font-lock-comment-face))))
     (org-agenda-structure ((t (:inherit font-lock-comment-face))))
     (compilation-info ((t (:inherit font-lock-comment-face))))
     (custom-variable-tag ((t (:inherit font-lock-comment-face :weight bold))))
     (custom-face-tag-face ((t (:inherit custom-variable-tag))))
     (eshell-ls-directory ((t (:inherit font-lock-comment-face))))
     (ido-subdir ((t (:inherit font-lock-comment-face))))
     (dired-directory ((t (:inherit font-lock-comment-face))))
     (slime-repl-prompt-face ((t (:inherit font-lock-comment-face))))
     (nxml-comment-content-face ((t (:inherit font-lock-comment-face))))

;; Strong text highlight 1 (white)
     (ido-first-match ((t (:foreground "white"))))
     (show-paren-match-face ((t (:inherit ido-first-match :weight bold))))
     (calendar-today ((t (:inherit ido-first-match))))
     (org-ellipsis ((t (:inherit ido-first-match))))
     (eshell-prompt ((t (:inherit ido-first-match))))
     (speedbar-highlight-face ((t (:inherit ido-first-match :weight bold))))
     (dired-header ((t (:inherit ido-first-match))))
     (link ((t (:inherit ido-first-match :underline t))))
     (widget-link ((t (:inherit link))))
     (custom-link ((t (:inherit link))))
     (org-link ((t (:inherit link))))
     ;; (modeline-buffer-id ((t (:inherit ido-first-match))))


;; Strong text highlight 2 (light green)
     (font-lock-type-face ((t (:foreground "#b9FC6D"))))
     (org-level-5 ((t (:inherit font-lock-type-face))))
     (ido-only-match ((t (:inherit font-lock-type-face))))
     (mode-line-highlight ((t (:inherit font-lock-type-face))))
;; TODO this needs to be distinct but not distracting, and work fine inactive

     (custom-documentation ((t (:inherit font-lock-type-face))))
     (speedbar-tag-face ((t (:inherit font-lock-type-face :height 0.9))))
     (slime-repl-inputed-output-face ((t (:inherit font-lock-type-face))))
     (info-xref-face ((t (:inherit font-lock-type-face))))

;; Subdued text highlighting 1
     (font-lock-doc-face ((t (:foreground "#777"))))
     (minibuffer-prompt ((t (:inherit font-lock-doc-face))))
     (minibuffer-noticeable-prompt ((t (:inherit font-lock-doc-face))))
     (modeline-mousable ((t (:inherit font-lock-doc-face))))
     (modeline-mousable-minor-mode ((t (:inherit font-lock-doc-face))))
     (custom-state ((t (:inherit font-lock-doc-face))))
     (org-date ((t (:inherit font-lock-doc-face))))


;; Subdued text highlighting 2

     (org-table ((t (:foreground "#555"))))
     (org-time-grid ((t (:inherit org-table))))
     (org-done ((t (:inherit org-table :weight bold))))
     (org-headline-done ((t (:inherit org-table))))
     (speedbar-separator-face ((t (:inherit org-table :background "#555" :height 0.9))))
     (nxml-delimiter-face ((t (:inherit org-table))))
     (nxml-element-local-name-face ((t (:inherit org-table :weight normal))))
     (nxml-tag-slash-face ((t (:inherit org-table))))

;; light red
       (font-lock-keyword-face ((t (:foreground "#ff5555"))))
       (org-todo ((t (:inherit font-lock-keyword-face))))
       (org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))

;; Strong but not overbearing (dark blue)
     (font-lock-builtin-face ((t (:foreground "#5555ff"))))
     (org-level-1 ((t (:inherit font-lock-builtin-face))))

;; dark purple
     (font-lock-constant-face ((t (:foreground "#9651cc"))))
     (org-level-6 ((t (font-lock-constant-face))))
     (css-property ((t (:inherit font-lock-constant-face))))

;; light yellow
     (font-lock-string-face ((t (:foreground "#ffff55"))))
     (org-level-2 ((t (:inherit font-lock-string-face :weight bold))))
     (css-selector ((t (:inherit font-lock-string-face))))
     (nxml-attribute-value-face ((t (:inherit font-lock-string-face))))
     ;; TODO refactor to big heading
     (custom-group-tag ((t (:inherit font-lock-string-face
			    :height 1.2 :weight bold))))


;; bright purple
     (font-lock-variable-name-face ((t (:foreground "#ff55ff"))))
     (org-level-4 ((t (:inherit font-lock-variable-name-face))))

;; Bright box highlight, you could try #555 instead of #777
       (isearch ((t (:background "#777" :foreground "white"))))
       (highlight ((t (:inherit isearch))))
       (match ((t (:inherit isearch))))
       (speedbar-selected-face ((t (:inherit isearch :weight bold :height 0.9))))
       (search-buffers-face ((t (:inherit isearch))))
       (tabbar-selected-face ((t (:inherit isearch :height 0.8))))

;; Subdued box highlight 1
       (region ((t (:background "#333"))))
       (secondary-selection ((t (:inherit region))))
       (modeline ((t (:inherit region))))
       (lazy-highlight-face ((t (:inherit modeline))))
       (isearch-lazy-highlight-face ((t (:inherit modeline))))
       (dired-flagged ((t (:inherit modeline))))
       (tabbar-unselected-face ((t (:inherit modeline :height 0.8))))
       (tool-bar ((t (:inherit modeline))))
       (menu ((t (:inherit modeline))))
       (custom-button ((t (:inherit modeline :weight bold))))
       (custom-button-pressed ((t (:inherit custom-button))))
       (custom-button-mouse ((t (:inherit region :foreground "white" :weight bold))))
       (flyspell-incorrect ((t (:inherit region))))
       (flyspell-duplicate ((t (:inherit region))))
       (search-buffers-header-face ((t (:inherit modeline))))
       (header-line ((t (:inherit modeline))))

;; Subdued box highlight 2
       (highline-face ((t (:background "#222"))))
       (widget-field-face ((t (:inherit highline-face :foreground "#555" ))))
       (org-column ((t (:inherit highline-face :foreground "white"))))
       (tabbar-separator-face ((t (:inherit highline-face :height 0.7))))
       ;; Needs to inherit from variable pitch to avoid "---------"
       (tabbar-default-face ((t (:inherit variable-pitch :background "#222" :height 0.8))))
       (tabbar-button-face ((t (:inherit highline-face :height 0.8))))
     



;; Fail/warning
       (isearch-fail ((t (:foreground "#ee3c3c" :underline t))))
       (show-paren-mismatch-face ((t (:inherit isearch-fail))))
       (org-warning ((t (:inherit isearch-fail :underline nil))))
       (eshell-ls-archive ((t (:inherit isearch-fail :underline nil))))
       (rng-error-face ((t (:inherit isearch-fail))))

;; Green
       (org-scheduled ((t (:foreground "#00bb00"))))
       (org-scheduled-previously ((t (:inherit org-scheduled))))
       (org-special-keyword ((t (:inherit org-scheduled))))
       (nxml-attribute-local-name-face ((t (:inherit org-scheduled))))

;; Small font
       (speedbar-file-face ((t (:height 0.9))))

;; Faces no thank you
       (org-done-headline ((t ())))
       (mlinks-link ((t ())))
       (nxml-prolog-keyword-face ((t ())))
       (nxml-prolog-literal-content-face ((t ())))
       (nxml-text-face ((t ())))

;; Background color
       (org-hide ((t (:foreground "black"))))

;; Misc
       (tooltip ((t (:foreground "#333"))))
       (mode-line-inactive ((t (:background "#111" :foreground "#444"))))
       (speedbar-directory-face ((t (:background "grey10" :foreground "grey50" :height 0.9))))

))


;;-----------------------------------------------------------------------------
;; Console
;;-----------------------------------------------------------------------------
;; This will only look good with the colors mentioned in Overview
     (color-theme-install
      '(color-theme-colorful-obsolescence
	
       ((background-color . "black")
	(background-mode . dark)
	(foreground-color . "white"))
       (default ((t (nil))))
      (custom-group-tag ((t (:foreground "white" :weight bold))))
      (font-lock-comment-face ((t (:foreground "blue" :weight bold))))
      (font-lock-keyword-face ((t (:foreground "red" :weight bold))))
      (font-lock-string-face ((t (:foreground "yellow" :weight bold))))
      (font-lock-variable-name-face ((t (:foreground "magenta" :weight bold))))
      (font-lock-constant-face ((t (:foreground "magenta" :weight bold))))
      (font-lock-doc-face ((t (:foreground "cyan"))))
      (isearch ((t (:background "cyan" :foreground "white" :weight bold))))
      (isearch-lazy-highlight ((t (:background "magenta" :foreground "white" ))))
      (highlight ((t (:background "cyan" :foreground "white" :weight bold))))
      (region ((t (:background "magenta"))))
      (lazy-highlight-face ((t (:background "magenta" :foreground "white"))))
      (ido-subdir-face ((t (:foreground "blue" :weight bold))))
      (isearch ((t (:background "cyan" :foreground "white" :weight bold))))
      (isearch-lazy-highlight ((t (:background "magenta" :foreground "white"))))
      (menu ((t (:background "black" :weight bold))))
      (mode-line ((t (:background "black"))))
      (org-date ((t (:foreground "cyan" :underline t :weight bold))))
      (org-level-1 ((t (:foreground "blue"))))
      (org-level-3 ((t (:foreground "blue" :weight bold))))
      (org-level-4 ((t (:foreground "magenta" :weight bold))))
      (org-link ((t (:foreground "white" :weight bold))))
      (org-scheduled-previously ((t (:foreground "green"))))
      (org-scheduled-today ((t (:foreground "green" :weight bold))))
      (org-special-keyword ((t (:foreground "green" :weight bold))))
      (org-table ((t (:foreground "black" :weight bold))))
      (org-tag ((t (:foreground "green" :underline t :weight bold))))
      (org-time-grid ((t (:foreground "black" :weight bold))))
      (org-todo ((t (:foreground "red" :weight bold))))
      (org-upcoming-deadline ((t (:foreground "red"))))
      (show-paren-match-face ((t (:foreground "white" :weight bold))))
      (eshell-prompt-face ((t (:foreground "white" :weight bold))))
      (eshell-ls-archive-face ((t (:foreground "red"))))
      (eshell-ls-directory-face ((t (:foreground "blue" :weight bold))))
      ))

     )))
