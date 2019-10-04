;;;;UDP STUFF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(in-package #:x-control)
(defun receive-data (udp-port &key (debug nil))
  "Used to receive coordinate data from the android phone over UDP"
  (let ((connection  (usocket:socket-connect nil nil
					     :local-host *IP*
					     :local-port udp-port
					     :protocol :datagram
					     :element-type '(unsigned-byte 8))))
    (unwind-protect (loop
		      (if (usocket:wait-for-input connection)
			  (let* ((orig (usocket:socket-receive connection nil 250))
				 (json (cl-json:decode-json-from-string
					(map 'string #'code-char orig))))
				(when debug
				  (format t "JSON: ~A~%" json))
			    (execute-json json))))

      (when connection (usocket:socket-close connection)))))

;;;((:RESOLUTION (:X . 720) (:Y . 1280)))

