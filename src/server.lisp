(in-package #:x-control)
;;;;this bad boi right here is the main file that contains all the functions related to the
;;;;threading activity of the server
;;;;its pretty simple, there is a tcp in con, tcp out con and a udp in con all handled
;;;;in seperate threads, this bad boi sets the whole thing rolling.


(defparameter *udp-port* nil);;23455 temp port
(defparameter *tcp-port-input* 60000);;60000 is default
(defparameter *tcp-port-output* nil);;23457
(defparameter *IP* "192.168.200.9")
(defparameter *debug-output* t)


(defun make-client (ip &key (tcp-port 60000) (udp-port 23455) (debug nil))
  (wait-for-connection-from-phone 
   (make-instance 'client :ip ip :tcp-port tcp-port :udp-port udp-port :debug-program debug)))

(defmethod shutdown-client ((client client))
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

(defmethod wait-for-connection-from-phone ((client client))
  (set-tcp-listening client)
  (handler-case 
      (unwind-protect (progn
                        (set-tcp-listening client)
                        (accept-connection client)
                        (get-and-set-remote-information client)
                        (create-stream client)
                        (get-and-set-ips client)
                        (set-input-udp-connection client)
                        (set-output-tcp-connection client)
                        ;;                        client
                        (get-and-set-ips client)
                        (start-master-loop client))
        (shutdown-client client))
    (SB-INT:SIMPLE-STREAM-ERROR (c)
      (shutdown-client client)
      (values client c))
    (END-OF-FILE (c)
      (shutdown-client client)
      (values client c))))

(defmethod start-master-loop ((client client))
  (let ((remote (remote-stream client))
        (udp (input-udp-connection client)))
    
    (loop
      (when (listen remote)
        (let ((json (json:decode-json remote)))
          (when (debug-program client)
            (format *standard-output* "~%json: ~A~%" json))
          (execute-json client json)))
      (when (usocket:wait-for-input udp :timeout 0.1)
        (let* ((orig (usocket:socket-receive udp nil 250))
               (json (cl-json:decode-json-from-string
                      (map 'string #'code-char orig))))
          (when (debug-program client)
            (format t "JSON: ~A~%" json))
          (execute-json client json))))))
