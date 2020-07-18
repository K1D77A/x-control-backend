;;;;the functions to handle tcp connections which are sent recieved with json.
(in-package #:x-control)
;;;;~~~~~~~~~~~~~~~~~~~ TCP Resolution Reception
(defparameter *REMOTE-PEER-IP* ())
(defparameter *REMOTE-SERVER-CONNECTION-STREAM* ());;need this to be global so that
;;it can be passed around easily
(defmethod set-tcp-listening ((client client))
  "Creates a listening TCP connection using the ip and tcp-port values in CLIENT."
  (handler-case
      (setf (tcp-server client)(usocket:socket-listen  (ip client) 
                                                       (tcp-port client) 
                                                       :reuse-address t
                                                       :reuseaddress t))
    (USOCKET:ADDRESS-IN-USE-ERROR ()
      (format t "~&Address is in use.~%"))))

(defmethod accept-connection ((client client))
  (setf (remote-connection client) (usocket:socket-accept (tcp-server client))))

(defmethod get-and-set-remote-information ((client client))
  (setf (remote-ip client) (get-peer-address (remote-connection client))
        (remote-tcp-port client) (get-peer-port (remote-connection client))))

(defmethod create-stream ((client client))
  (setf (remote-stream client) (usocket:socket-stream
                                (usocket:wait-for-input (remote-connection client)))))

(defmethod get-and-set-ips ((client client))
  (loop :for x :from 0 :to 1000
        :if (remote-stream client)
          :do 
             (let ((json (json:decode-json (remote-stream client))))
               (format *standard-output* "~%json: ~A~%" json)
               (execute-json client json)
               (return))
        :else :do (sleep 0.001)
        :finally (signal-no-input-from-device
                  :setip "((TCPINPUT . <n>) (UDPPORT . <n>)" "no :setip command sent")))

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
               )
          (usocket:socket-close tcp-in-socket))
      (SB-INT:SIMPLE-STREAM-ERROR (condition)
        ;;catching a stream closed by other device error
        (dc condition))
      (END-OF-FILE (condition)
        ;;catching a end of file for the stream error
        (dc condition)))))

(defmethod set-output-tcp-connection ((client client))
  (print (output-tcp-port client))
  (loop :for x :from 0 :to 5;;timeout after 5 seconds ie 5 tries
        :if
        (handler-case  
            (unwind-protect
                 (progn
                   (setf (output-tcp-connection client)
                         (usocket:socket-connect (remote-ip client)
                                                 (output-tcp-port client)
                                                 :protocol :stream))
                   (return))
              
              ;;need to change this stuff to a shutdown function
              (shutdown-client client))
          (USOCKET:CONNECTION-REFUSED-ERROR ()
            t))
        :do (sleep 1)
        :finally (error "Failed to establish a TCP connection to the device")))

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


