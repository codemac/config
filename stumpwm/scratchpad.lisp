(in-package :cl-user)
(defpackage net.codemac.stumpwm.scratchpad
  (:use :cl :stumpwm :net.codemac.stumpwm.window)
  (:export #:*default-split-ratio*
           #:*default-float-ratio*
           #:toggle-split-scratchpad
           #:toggle-floating-scratchpad))
(in-package :net.codemac.stumpwm.scratchpad)

(defvar *default-split-ratio* 1/2)

(defvar *default-float-ratio* 1/2)

(defvar *scratch-floats* '()
  "Alist of names to windows for float scratchpads.")

(defun scratchpad-handle-float-window-destroy (window)
  (setf *scratch-floats* (delete window *scratch-floats*
                                 :key #'cdr)))

(remove-hook *destroy-window-hook* #'scratchpad-handle-float-window-destroy)
(add-hook *destroy-window-hook* #'scratchpad-handle-float-window-destroy)

(defvar *scratch-splits* '()
  "Alist of names to windows for split scratchpads.")

(defun scratchpad-handle-split-window-destroy (window)
  (setf *scratch-splits* (delete window *scratch-splits*
                                 :key #'cdr)))

(remove-hook *destroy-window-hook* #'scratchpad-handle-split-window-destroy)
(add-hook *destroy-window-hook* #'scratchpad-handle-split-window-destroy)

(defun maybe-remove-old-split (moved-from-group moved-from-frame)
  "Remove old frame if empty."
  (let* ((head (stumpwm::frame-head moved-from-group moved-from-frame))
         (tree (stumpwm::tile-group-frame-head moved-from-group head)))
    (when (and (null (stumpwm::frame-window moved-from-frame))
               (not (atom tree)))
      (remove-split moved-from-group))))

(defun resize-by-gravity (window gravity ratio)
  (let* ((screen (current-screen))
         (screen-x (stumpwm::screen-x screen))
         (screen-y (stumpwm::screen-y screen))
         (screen-width (stumpwm::screen-width screen))
         (screen-height (stumpwm::screen-height screen))

         (x-min screen-x)
         (x-max (- (+ screen-x screen-width)
                   (* 2 stumpwm::*float-window-border*)))

         (y-min screen-y)
         (y-max (- (+ screen-y screen-height)
                   stumpwm::*float-window-title-height*
                   stumpwm::*float-window-border*))

         (new-x-min x-min)
         (new-x-max x-max)
         (new-y-min y-min)
         (new-y-max y-max)
         (x-width (- x-max x-min))
         (x-ratio-length (- x-width (floor (* x-width ratio))))
         (y-width (- y-max y-min))
         (y-ratio-length (- y-width (floor (* y-width ratio)))))
    (when (member gravity '(:top :top-right :top-left))
      (decf new-y-max y-ratio-length))

    (when (member gravity '(:bottom :bottom-right :bottom-left))
      (incf new-y-min y-ratio-length))

    (when (member gravity '(:left :top-left :bottom-left))
      (decf new-x-max x-ratio-length))

    (when (member gravity '(:right :top-right :bottom-right))
      (incf new-x-min x-ratio-length))

    (when (eq gravity :center)
      (decf new-y-max (floor (/ y-ratio-length 2)))
      (incf new-y-min (floor (/ y-ratio-length 2)))

      (incf new-x-min (floor (/ x-ratio-length 2)))
      (decf new-x-max (floor (/ x-ratio-length 2))))

    (stumpwm::float-window-move-resize window
                                       :x new-x-min :y new-y-min
                                       :width (- new-x-max new-x-min 1)
                                       :height (- new-y-max new-y-min 1))))

(defun flatten-direction (current-frame gravity)
  "Convert direction list into single direction."
  (let ((direction '()))
    (when (member gravity '(:top :top-right :top-left))
      (push :above direction))
    (when (member gravity '(:bottom :bottom-right :bottom-left))
      (push :below direction))
    (when (member gravity '(:left :top-left :bottom-left))
      (push :left direction))
    (when (member gravity '(:right :top-right :bottom-right))
      (push :right direction))
    (cond ((and (listp direction)
                (= 1 (length direction)))
           (first direction))
          ((listp direction)
           (let* ((w (frame-width current-frame))
                  (h (frame-height current-frame))
                  (allowed (if (< w h)
                               '(:above :below)
                               '(:left :right))))
             (or (first (intersection allowed direction))
                 :below)))
          (t
           direction))))

(defun scratchpad-split-frame (gravity ratio group current-frame current-window
			       scratchpad-window moved-from-group moved-from-frame)
  "Create a new frame and place the scratchpad in it."
  (let* ((decided-direction (flatten-direction current-frame gravity))
         (swapped (member decided-direction '(:above :left)))
         (dir (if (member decided-direction '(:below :above)) :row :column))
         (r (if swapped ratio (- 1 ratio)))
         (old-num (stumpwm::frame-number current-frame))
         (new-num (stumpwm::split-frame group dir r))
         (target-frame (stumpwm::frame-by-number group (if swapped old-num new-num)))
         (original-frame (stumpwm::frame-by-number group (if swapped new-num old-num))))
    (move-window-to-group scratchpad-window group)
    (maybe-remove-old-split moved-from-group moved-from-frame)
    (when swapped
      (stumpwm::migrate-frame-windows group target-frame original-frame))
    (stumpwm::pull-window scratchpad-window target-frame nil)
    (when current-window
      (stumpwm::pull-window current-window original-frame nil))
    (stumpwm::focus-frame group target-frame)
    (stumpwm::sync-all-frame-windows group)))

(defun toggle-split-scratchpad (name cmd &key (ratio *default-split-ratio*)
					   (gravity :bottom-right)
					   (all-groups *run-or-raise-all-groups*)
					   (all-screens *run-or-raise-all-screens*))
  "Create or toggle display of a named scratchpad.  Display by creating a frame."
  (let ((found (member name *scratch-splits*
                       :key #'car
                       :test #'string=)))
    (cond ((and found
                (not (typep (cdr (car found))
                            'stumpwm::tile-window)))
           ;; Type not correct!
           nil)
          (found
           (let* ((window (cdr (car found)))
                  (group (current-group))
                  (current-frame (stumpwm::tile-group-current-frame group))
                  (current-window (group-current-window group))
                  (moved-from-frame (stumpwm::window-frame window))
                  (moved-from-group (window-group window)))
             (cond
               ;; Currently focused on scratchpad; Hide it
               ((and (eq current-frame moved-from-frame)
                     (eq window (stumpwm::frame-window current-frame)))
                (remove-split))
               ;; Scratchpad is visible, move to it
               ((and (eq moved-from-group group) (window-visible-p window))
                (stumpwm::focus-frame moved-from-group moved-from-frame))
               ;; Current frame is empty, just display it
               ((null (stumpwm::frame-window current-frame))
                (move-window-to-group window group)
                (maybe-remove-old-split moved-from-group moved-from-frame))
               ;; Scratchpad needs a new frame
               (t (scratchpad-split-frame gravity
                                          ratio
                                          group
                                          current-frame
                                          current-window
                                          window
                                          moved-from-group
                                          moved-from-frame)))))
          (t
           (with-new-window (window cmd)
             :new
             (push (cons name window) *scratch-splits*))))))

(defun toggle-floating-scratchpad (name cmd &key initial-gravity (ratio *default-float-ratio*))
  "Create or toggle display of a named scratchpad.  Display by floating window."
  (let ((found (member name *scratch-floats*
                       :key #'car
                       :test #'string=)))
    (cond ((and found
                (not (typep (cdr (car found))
                            'stumpwm::float-window)))
           ;; Type not correct!
           nil)
          (found
           (let ((window (cdr (car found))))
             (cond ((eq (current-window) window)
                    (stumpwm::hide-window window))
                   ((stumpwm::window-in-current-group-p window)
                    (focus-window window t))
                   (t
                    (move-window-to-group window (current-group))
                    (focus-window window t)))))
          (t
           (with-new-window (window cmd)
             :new
             (push (cons name window) *scratch-floats*)
             :focus
             (stumpwm::float-window window (current-group))
             (cond (initial-gravity
                    (resize-by-gravity window initial-gravity ratio))))))))

(defcommand scratchpad-float (name cmd gravity) ((:string "Name: ")
                                                 (:string "Command: ")
                                                 (:gravity "Side: "))
  (toggle-floating-scratchpad name cmd
			      :initial-gravity gravity))
