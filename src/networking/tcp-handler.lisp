;;;;the functions to handle tcp connections which are sent recieved with json.
(in-package #:x-control)
;;;;~~~~~~~~~~~~~~~~~~~ TCP Resolution Reception
(defparameter *REMOTE-PEER-IP* ())
(defparameter *REMOTE-SERVER-CONNECTION-STREAM* ());;need this to be global so that
;;it can be passed around easily
(defun establish-tcp-listen (port)
  "Creates a listening TCP connection and returns the object for use in multiple functions"
  (let ((connection (usocket:socket-listen  *IP*
					    port
					    :reuse-address t
					    :reuseaddress t)))
    connection))



(defun maintain-connection-input ()
  "Creates a contantly listening server for the device to connect to, on *tcp-port-input*, this function also sets the value of *remote-peer-ip* for use with (create-output-connection)"
  (flet ((dc (condition)
	   (progn (format *standard-output*
			  "~%EOF: ~A~%Client has disconnected or an error has occurred~%" condition)
		  (execute ':shutdown nil)
		  (wait-to-play))))
    (declare (special tcp-in-socket))
    (setf tcp-in-socket (establish-tcp-listen *tcp-port-input*))
    (handler-case
	(unwind-protect
	     (let* ((accepted (usocket:socket-accept tcp-in-socket))
		    (rpi (usocket:get-peer-name accepted)))
	       (format *standard-output* "~%Remote PEER ~A~%" rpi)		   
	       (unless (or (null rpi) *REMOTE-PEER-IP*)
		 (setf *REMOTE-PEER-IP* rpi))
	       (loop
		 (let ((stream
			 (usocket:socket-stream (usocket:wait-for-input accepted))))
		   (when stream
		     (let ((json (json:decode-json stream)))
					;(format *standard-output* "~%json: ~A~%" json)
		       (execute-json json))))))
	  (usocket:socket-close tcp-in-socket))
      (SB-INT:SIMPLE-STREAM-ERROR (condition)
	;;catching a stream closed by other device error
	(dc condition))
      (END-OF-FILE (condition)
	;;catching a end of file for the stream error
	(dc condition)))))

   
(defun create-output-connection (tcp-port-output)
  "Creates a valid connected socket and sets *REMOTE-SERVER-CONNECTION-STREAM* to it. Uses
the value of *remote-peer-ip* which is set by (maintain-connection-input) if this value has not been set, this function will just loop until it is eventually set. The value of *remote-server-connection-stream* being set to a valid socket is vital for the operation of the program "
      (handler-case  
	  (unwind-protect
	       (let ((socket (socket-connect *REMOTE-PEER-IP* tcp-port-output :protocol :stream )))
		 (setf *REMOTE-SERVER-CONNECTION-STREAM* socket))
	    (when *REMOTE-SERVER-CONNECTION-STREAM*
	      (usocket:socket-close *REMOTE-SERVER-CONNECTION-STREAM*)))
	(USOCKET:CONNECTION-REFUSED-ERROR (condition)
	  (declare (ignore condition))
	  (format *standard-output* "Retrying connection to remote in 1 second~%")
	  (sleep 1)
	  (create-output-connection tcp-port-output))))
    
 ;  (json:decode-json stream)
;(defun clean-input (string)
;  (loop
(defun connect-tcp (tcp-listening-stream)
  "testing function to send json over"
  (let ((port (usocket:get-local-port tcp-listening-stream))
	(ip (usocket:get-local-name tcp-listening-stream)))
    (usocket:socket-connect ip port)))
(defun get-tcp-information (connection)
  (let* ((accepted (usocket:socket-accept connection))
	 (remote-port (usocket:get-peer-port accepted))
	 (remote-host (usocket:get-peer-name accepted)))
    (format t "port ~A~%host ~A~%" remote-port remote-host)
    accepted))
(defun send-json (connection alist)
  (json:encode-json-alist alist connection)
  (force-output connection))


