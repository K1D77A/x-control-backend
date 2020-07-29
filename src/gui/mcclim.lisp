(defpackage #:x-control.gui
  (:use #:clim #:clim-lisp)
  (:export #:start-gui))
(in-package #:x-control.gui)


#|
1.command is acquired
2.arguments required for command are acquired. args are often associated with a presentation type
clicking one can satisfy these
3.the command is called on the arguments 
4.a display routine is called to update the views of the application logic

writing an app consists of:
writing CLIM commands that modify the app data
writing display routines that turn the app structures into a collection of visable representations
writing completion routiens that allow you to type in application objects using completions
independently deciding how commands are to be invoked, menus, buttons etc


terminology:
app frame - usually called an application

panes - descibe an applications frames visual building blocks, side bar, menu bar, tables
used to compose the top-level user interface. can also be used to layout other panes. 
panes are the series of operations that produce that appearance. They keep a history of the steps
taken to generates an appearance

gadgets - called widgets/controls in other toolkits. buttons or sliders are gadgets

hierarchy of panes and gadgets (gadgets are a special kind of pane)


|#


;;; constructor for the PERSON class. Not strictly necessary.


(defun make-button (label operator &key width height
                                     (max-width +fill+) min-width
                                     (max-height +fill+) min-height)
  (make-pane 'push-button
             :label label
             :activate-callback operator
             :width width :height height
             :max-width  max-width :min-width min-width
             :max-height max-height :min-height min-height))

(define-application-frame x-control ()
  ((%client :initform (make-instance 'x-control::client)
            :accessor client)
   (%client-thread :accessor client-thread)
   (%console-stream :accessor console-stream :initform (make-string-output-stream))
   (%console-stream-lock :accessor console-stream-lock :initform (bt:make-lock)))
  (:menu-bar t)
  (:panes
   (main-pane :application :height 250 :width 250)
   (debug-output :application :height 250 :width 250
                              :display-function #'debug-display-function)
   (console-out :application :height 50 :width 250                
                             :display-function #'console-display-function)
   (shutdown (make-button "shutdown" #'shutdown-the-client))
   (connect (make-button "connect" #'wait-for-connection))
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default (vertically (:width 1000 :height 500)
              main-pane
              debug-output
              console-out
              (tabling (:grid t)
                (list connect shutdown))
              interactor))))

(defun console-display-function (frame pane)
  (with-accessors ((console-stream console-stream)
                   (console-stream-lock console-stream-lock))
      frame
    (bt:with-lock-held (console-stream-lock)
      (incf *oof*)
      (let* ((stream  (get-output-stream-string console-stream)))
        (format pane "~A~%" stream)))))
(defparameter *oof* 0)

(defun debug-display-function (frame pane)
  (bt:with-lock-held ((x-control::output-stream-lock (client frame)))
    (let ((stream (make-string-input-stream (get-output-stream-string
                                             (x-control::output-stream (client frame))))))
      (format pane "~S~%" (read-line stream nil)))))
;;(format t "~A" stream)
;; (loop :for line := (read-line stream nil)
;;      :while line
;;    :do (format pane "~S~%" line)))))


(defun shutdown-the-client (gadget)
  (declare (ignore gadget))
  (with-accessors ((client client)
                   (console-stream console-stream)
                   (console-stream-lock console-stream-lock)
                   (client-thread client-thread))
      *application-frame*
    (x-control::shutdown-client client)
    (bt:with-lock-held (console-stream-lock)
      (format console-stream "Shutdown complete~%"))
    (sleep 0.1)
    (bt:destroy-thread client-thread)))

(defun wait-for-connection (gadget)
  (declare (ignore gadget))
  (with-accessors ((client client)
                   (thread client-thread)
                   (console-stream console-stream))
      *application-frame*
    (with-accessors ((ip ip)
                     (tcp-port tcp-port)
                     (udp-port udp-port))
        client
      (unless (x-control::runningp client)
        (if (every (lambda (slot)
                     (slot-boundp client slot))
                   '(x-control::ip x-control::tcp-port x-control::udp-port))
            (setf thread
                  (bt:make-thread (lambda ()
                                    (x-control::wait-for-connection-from-phone client))
                                  :name "connection-thread"))
            (format console-stream "ip, tcp-port or udp-port is not set~%"))))))


(defun app-main ()
  (let ((frame (make-application-frame 'x-control)))
    (run-frame-top-level frame)
    frame))
