(in-package #:x-control)
;;;;doing the clx stuff

(defparameter *current-x* 0)
(defparameter *current-y* 0)


(defparameter *android-x-res* 0)
(defparameter *android-y-res* 0)
(defparameter *display* (xlib:open-default-display))
(defparameter *screen* (xlib:display-default-screen *display*))
(defparameter *window* (xlib:screen-root *screen*))

(defun setup-x-control ()
  (setf *display*
        (xlib:open-default-display))
  (setf (xlib:display-report-asynchronous-errors *display*) '(:NEVER))
  (setf *screen*
        (xlib:display-default-screen *display*))
  (setf *window*
        (xlib:screen-root *screen*))
  (wait-to-play)
  (sb-impl::toplevel-init))

(defun set-current-position (x y)
  "sets the values of *current-x* and *current-y* to x and y"
  (setf *current-x* x
        *current-y* y))

(defun set-xy-to-mouse-position-now ()
  "sets *current-x* and *current-y* to the mouse pointers x y position at time of calling"
  (multiple-value-bind (x y)
      (xlib:global-pointer-position *display*)
    (set-current-position x y)))

;; (defun move-mouse (display x y)
;;   "moves the mouse on window to coords x y"
;;   (multiple-value-bind (cx cy)
;;       (xlib:global-pointer-position display)
;;     (let* ((inx (floor(* 1920/1080 x)))
;; 	   (iny (floor(* 1920/1080 y)))
;; 	   (dx (- inx cx))	  
;; 	   (dy (- iny cy)))
;;       (xlib:warp-pointer-relative display dx dy)
;;       (xlib:display-force-output display))))

(defun move-mouse (display x y)
  "moves the mouse relative to the pointer positions stored in *current-x* and *current-y*"
  (xlib:warp-pointer *window*
                     (- *current-x* x)
                     (- *current-y* y))
  (xlib:display-force-output display))

(defun query-for-physical-screens (display)
  "Returns the number of screens connected to the host machine"
  (xlib/xinerama:xinerama-query-screens display))

(defun perform-mouse-action (press? button)
  (ignore-errors (progn
                                        ; (xlib/xtest:grab-control *display* t)
                   (xlib/xtest:fake-button-event *display* button press? )
                   (xlib:display-force-output *display*))))
                                        ; (xlib:value-error () nil)))

(defun up ()
  (perform-mouse-action nil 1))

(defun down ()
  (perform-mouse-action t 1))

(defun left-click ()
  (unwind-protect
       (progn 
         (down)
         (up))
    (up)))

(defun right-click ()
  "Performs a right-click"
  (perform-mouse-action t 3)
  (perform-mouse-action nil 3))

(defun char-to-keycode (char display)
  "Takes in a char and converts it to a keycode which can be used in a simulated input event"
  (xlib:keysym->keycodes display (first (xlib:character->keysyms char))))

(defun string-to-fake-events (string display &key (delay 0))
  "Takes in a string which is then used to create a fake keyboard event which is sent to the x11 server"
  (map 'list
       (lambda (char)
         (let ((keycode (char-to-keycode char display)))
           (xlib/xtest:fake-key-event display
                                      keycode
                                      t
                                      :delay delay)
           (xlib/xtest:fake-key-event display
                                      keycode
                                      nil
                                      :delay delay)))
	   
       string)
  (xlib:display-force-output display))

