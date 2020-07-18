(in-package #:x-control)
;;;;this bad boi right here is the main file that contains all the functions related to the
;;;;threading activity of the server
;;;;its pretty simple, there is a tcp in con, tcp out con and a udp in con all handled
;;;;in seperate threads, this bad boi sets the whole thing rolling.

(defun get-interface-ips ()
  (ip-interfaces:get-ip-interfaces))

(defun make-client (ip &key (tcp-port 60000) (udp-port 23455) (debug nil))
  (wait-for-connection-from-phone 
   (make-instance 'client :ip ip :tcp-port tcp-port :udp-port udp-port :debug-program debug)))

(defmethod shutdown-client ((client client))
  "Attempts to close all the sockets associated with CLIENT."
  (handler-case (when (remote-connection client)
                  (usocket:socket-close (tcp-server client)))
    (unbound-slot (c)
      (values c client)))
  (handler-case (when (remote-connection client)
                  (usocket:socket-close (remote-connection client)))
    (unbound-slot (c)
      (values c client)))
  (handler-case (when (output-tcp-connection client)
                  (usocket:socket-close (output-tcp-connection client)))
    (unbound-slot (c)
      (values c client)))
  (handler-case (when (input-udp-connection client)
                  (usocket:socket-close (input-udp-connection client)))
    (unbound-slot (c)
      (values c client))))

(defun trace-all ()
  (trace set-tcp-listening
         accept-connection
         get-and-set-remote-information
         create-stream
         get-and-set-ips
         set-input-udp-connection
         set-output-tcp-connection
         start-master-loop))

(defmethod wait-for-connection-from-phone ((client client))
  "Given a CLIENT attempts to start the connection with the android device. Once connected it will
loop and execute the correct commands based on the information received both on a udp and tcp port
within client."
  (handler-case 
      (unwind-protect (progn
                        (set-tcp-listening client)
                        (accept-connection client)
                        (get-and-set-remote-information client)
                        (create-stream client)
                        (get-and-set-ips client)
                        (set-input-udp-connection client)
                        (set-output-tcp-connection client)
                        (get-and-set-ips client)
                        (start-master-loop client))
        (shutdown-client client))
    (unknown-error ()
      (shutdown-client client)
      (format t "~&Unknown error. Restarting~%")
      (make-client (ip client)
                   :tcp-port (tcp-port client) :udp-port (udp-port client)
                   :debug (debug-program client)))
    (condition (c)
      (shutdown-client client)
      (values client c))))

(defmethod start-master-loop ((client client))
  (let ((remote (remote-stream client))        
        (udp (input-udp-connection client)))
    (loop
      (when (listen remote)
        (let ((json (json:decode-json remote)))            
          (when (eq :shutdown (execute-json client json (debug-program client)))
            (shutdown-client client)
            (make-client (ip client)
                         :tcp-port (tcp-port client) :udp-port (udp-port client)
                         :debug (debug-program client)))))
      (when (wait-for-input udp :timeout 0.01 :ready-only t)
        (let* ((orig (usocket:socket-receive udp nil 250))
               (json (json:decode-json-from-string
                      (map 'string #'code-char orig))))            
          (execute-json client json (debug-program client)))))))
