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

(define-application-frame my-first-clim-app ()
  ()
  (:pointer-documentation t)
  (:panes
   (app :application 
        :height 200 
        :width 600
        :display-time nil)
   (interactor :interactor
               :height 200 
               :width 600))
  (:layouts
   (default (vertically ()
              app interactor))))


(define-my-first-clim-app-command (com-quit :name t) ()
  (frame-exit *application-frame*))
(define-my-first-clim-app-command (com-parity :name t) ((number 'integer))
  (format t "~A is ~A~%" number (if (oddp number)
                                    "odd"
                                    "even")))

(defun start-gui ()
  (run-frame-top-level (make-application-frame 'my-first-clim-app)))
