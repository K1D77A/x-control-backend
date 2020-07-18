;;;;UDP STUFF ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(in-package #:x-control)

(defmethod set-input-udp-connection ((client client))
  (setf (input-udp-connection client)
        (usocket:socket-connect nil nil
                                :local-host (ip client)
                                :local-port (input-udp-port client)
                                :protocol :datagram
                                :element-type '(unsigned-byte 8))))
