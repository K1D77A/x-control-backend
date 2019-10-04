(in-package #:x-control)
;;;;this bad boi right here is the main file that contains all the functions related to the
;;;;threading activity of the server
;;;;its pretty simple, there is a tcp in con, tcp out con and a udp in con all handled
;;;;in seperate threads, this bad boi sets the whole thing rolling.
(defun shut-remote-if-active ()
  "shuts down and resets the value of *remote-server-connection-stream* to nil if it isn't already"
  (unless (null *remote-server-connection-stream*)
    (usocket:socket-close *remote-server-connection-stream*)
    (setf *remote-server-connection-stream* nil)
    t))

(defun reset-vals ()
  "Just a helper function to help clean things up"
  (shut-remote-if-active)
  (setf *REMOTE-PEER-IP* nil)
  (setf *udp-port* nil)
  (setf *tcp-port-output* nil)
  (find-and-kill-thread "INPUT-COMMAND-DO"));;gotta deal with the fact this keeps getting made...

(defun debug-me ()
  (format t "*REMOTE-PEER-IP* ~A~%*REMOTE-SERVER-CONNECTION-STREAM* ~A~%"
	  *REMOTE-PEER-IP* *REMOTE-SERVER-CONNECTION-STREAM*))

(defun find-and-kill-thread (name)
  "finds and kills the thread 'name'"
  (let ((threads (bt:all-threads)))
    (mapcar (lambda (thread)
	      (when (string= (bt:thread-name thread)
			     name)
		(bt:destroy-thread thread)))
	    threads)))

(defun set-threads-to-std-out ()
   (setf bt:*default-special-bindings*;;this sets the var of standard out for the threads
	 (acons '*standard-output* *standard-output*
		bt:*default-special-bindings*)))

(defun kill-server-threads (calling-thread-name)
  "Kills all threads that are used for networking."
  (mapcar (lambda (thread)
	    (unless (string= thread calling-thread-name)
	      (find-and-kill-thread thread)))
	  (list "TCP-RECEIVE-THREAD" "UDP-DATA-THREAD"
		"SEND-COMMAND-OUT" "CREATE-OUTPUT-CONNECTION"))
  (shut-remote-if-active))


(defun kill-all-threads ()
  "Kills all threads including PARENT-THREAD"
  (kill-server-threads nil)
  (find-and-kill-thread "PARENT-THREAD")
  (sleep 0.1)
  (bt:all-threads))

(defun wait-to-play ()  
  "This is the big bad boi function, to start the server call this function, then when the phone connects it will connect and this will handle that connection start up the threads required and die. Then when the device disconnects this function will get recalled on a new thread to handle establishing new connections"  
  (declare (special tcp-in-socket))
  (reset-vals)
  (when (boundp (quote tcp-in-socket));;this may or may not be completely unbound
    (when tcp-in-socket
      (usocket:socket-close tcp-in-socket)))
  (let ((tcp-in-socket));;This is the var that contains the tcp-in socket
      (declare (ignore tcp-in-socket))
    (shut-remote-if-active)
    (set-threads-to-std-out)
    (bt:make-thread #'maintain-connection-input :name "TCP-RECEIVE-THREAD")))
