(in-package #:x-control)

(defclass client ()
  ((runningp
    :accessor runningp
    :initform nil)
   (udp-port 
    :accessor udp-port
    :initarg :udp-port
    :type integer)
   (tcp-port
    :accessor tcp-port
    :initarg :tcp-port
    :type integer)
   (ip
    :accessor ip
    :initarg :ip)
   (remote-ip
    :accessor remote-ip)
   (remote-tcp-port
    :accessor remote-tcp-port)
   (input-udp-port
    :accessor input-udp-port)
   (input-udp-connection
    :accessor input-udp-connection)
   (output-tcp-port
    :accessor output-tcp-port)
   (output-tcp-connection
    :accessor output-tcp-connection)
   (tcp-server
    :accessor tcp-server)
   (remote-connection
    :accessor remote-connection)
   (remote-stream
    :accessor remote-stream)
   (master-processor
    :accessor master-processor)
   (debug-program
    :accessor debug-program
    :initarg :debug-program
    :initform nil)
   (output-stream
    :accessor output-stream
    :initform (make-string-output-stream))
   (output-stream-lock
    :accessor output-stream-lock
    :initform (bt:make-lock))))

(define-condition keyword-not-found (condition)
  ((expected-keyword
    :accessor expected-keyword
    :initarg :expected-keyword
    :type keyword)
   (message
    :accessor message
    :initarg :message)))

(defun signal-keyword-not-found (keyword message)
  (error 'keyword-not-found :keyword keyword :message message))

(define-condition no-input-from-device (condition)
  ((expected-keyword
    :accessor expected-keyword
    :initarg :expected-keyword
    :type keyword)
   (expected-input
    :accessor expected-input
    :initarg :expected-input)
   (message
    :accessor message
    :initarg :message)))

(defun signal-no-input-from-device (keyword input message)
  (error 'no-input-from-device :message message :keyword keyword :input input))
