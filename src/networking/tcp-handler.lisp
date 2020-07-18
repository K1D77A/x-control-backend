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
