(in-package #:x-control)
;;;;THis file handles all the commands that are received from the android device


(defun keyword-from-json (json)
  (first (first json)))

(defun execute-json (json)
  (let ((keyword (keyword-from-json json)))
    (handler-case (execute keyword json)
      (SB-PCL::NO-PRIMARY-METHOD-ERROR (condition)
	(declare (ignore condition))
	(execute ':unknown json)))))

(defgeneric execute (keyword args)
  (:documentation
   "Executes a command depending on keyword received in the json sent from the
client"))

(defmethod execute ((keyword (eql ':unknown)) args))
		       
(defmethod execute :before (keyword args)
  (when *debug-output*
    (format *standard-output* "Function: '~A' Args: '~A'~%" keyword (rest (first args)))))

(defmethod execute ((keyword (eql ':position)) args)
   "moves the mouse to the x y on *display* and *window* both found in /src/x-control"
  (let* ((vals (assoc :position args));;this is just a precursory function
	 (x (cdr (second vals)));;it will probably be much more complex...
	 (y (cdr (third vals))))
    (move-mouse *display* x y)))

(defmethod execute ((keyword (eql ':setip)) args)
   "Takes in the tcp port used by the device to listen and the udp port selected by 
the device for sending packets and sets the values of *udp-port* *tcp-port-output*"
  (let* ((vals (assoc :setip args))
	 (tcp (cdr (second vals)))
	 (udp (cdr (third vals))))
    (bt:make-thread (lambda ()
		      (receive-data udp))
		    :name "UDP-DATA-THREAD")
    (bt:make-thread (lambda ()
		      (create-output-connection tcp))
		    :name "CREATE-OUTPUT-CONNECTION")))

(defmethod execute ((keyword (eql ':connected)) args)
   "Prints out the valid connection"
  (format *standard-output* "~%connection made~%"))
  
(defmethod execute ((keyword (eql ':shutdown)) args)
  "Shuts down all the connections, and set the server to listen again, basically starting a new loop"
  (bt:make-thread (lambda ()		   
		    (kill-all-threads);;Doesn't kill this thread
		    (set-threads-to-std-out)
		    (wait-to-play))))

(defmethod execute ((keyword (eql ':click)) args)
 "Handles a click command sent from the client"
    (down))

(defmethod execute ((keyword (eql ':unclick)) args)
    "Handles an unclick command sent from the client"
    (up))

(defmethod execute ((keyword (eql ':start-move)) args)
  "Handles a start-move event sent from client"
  (set-xy-to-mouse-position-now))

(defmethod execute ((keyword (eql ':right-click)) args)
   "Handles a right-click event sent from client"
  (right-click))

(defmethod execute ((keyword (eql ':text)) args)
   "Handles a text event sent from client"
  (when args
    (string-to-fake-events (rest (first args)) *display*)))



