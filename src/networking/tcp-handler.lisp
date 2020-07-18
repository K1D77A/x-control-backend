;;;;the functions to handle tcp connections which are sent recieved with json.
(in-package #:x-control)
;;;;~~~~~~~~~~~~~~~~~~~ TCP Resolution Reception
(defparameter *REMOTE-PEER-IP* ())
(defparameter *REMOTE-SERVER-CONNECTION-STREAM* ());;need this to be global so that
;;it can be passed around easily
(defmethod set-tcp-listening ((client client))
  "Creates a listening TCP connection using the ip and tcp-port values in CLIENT, and 
sets (tcp-server CLIENT) to to the new connection."
  (unwind-protect 
       (setf (tcp-server client)
             (usocket:socket-listen  (ip client) 
                                     (tcp-port client) 
                                     :reuse-address t
                                     :reuseaddress t))
    (shutdown-client client)))

(defmethod accept-connection ((client client))
  "Takes a CLIENT and sets (remote-connection CLIENT) to an accepted socket ie a connected socket."
  (setf (remote-connection client) (usocket:socket-accept (tcp-server client))))


(defmethod get-and-set-remote-information ((client client))
  "Takes a CLIENT and sets remote-ip and remote-tcp-port to the peer ip and peer port of 
(remote-connection CLIENT)."
  (setf (remote-ip client) (get-peer-address (remote-connection client))
        (remote-tcp-port client) (get-peer-port (remote-connection client))))

(defmethod create-stream ((client client))
  "Takes a CLIENT and sets (remote-stream CLIENT) to a socket-stream."
  (setf (remote-stream client) (usocket:socket-stream (remote-connection client))))

(defmethod get-and-set-ips ((client client))
  "Takes a CLIENT and receives a setip command from (remote-stream client) and executes it.
If it fails to receive this within 1 second, signals the condition 'no-input-from-device"
  (handler-case
      (loop :for x :from 0 :to 1000
            :if (and (remote-stream client) (listen (remote-stream client)))
              :do  (let ((json (json:decode-json (remote-stream client))))
                     (execute-json client json (debug-program client))
                     (return client))
            :else :do (sleep 0.001)
            :finally (signal-no-input-from-device
                      :setip "((TCPINPUT . <n>) (UDPPORT . <n>)" "no :setip command sent"))
    (stream-error (c)
      (shutdown-client client)
      (values client c))))
                  
    

(defmethod set-output-tcp-connection ((client client))
  (loop :for x :from 0 :to 5;;timeout after 5 seconds ie 5 tries
        :if
        (handler-case
            (progn
              (setf (output-tcp-connection client)
                    (usocket:socket-connect (remote-ip client)
                                            (output-tcp-port client)
                                            :protocol :stream))
              (return))
          
          ;;need to change this stuff to a shutdown function
          (USOCKET:CONNECTION-REFUSED-ERROR ()
            t))
        :do (sleep 1)
        :finally (error "Failed to establish a TCP connection to the device")))
