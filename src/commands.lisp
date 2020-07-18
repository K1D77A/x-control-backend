(in-package #:x-control)
;;;;THis file handles all the commands that are received from the android device

(defvar *keyword-fun-alist* ())

(defun key->fun (key)
  (let ((fun (second  (assoc key *keyword-fun-alist* :test #'eq))))
    (if fun
        fun 
        (signal-keyword-not-found key "no function found for key"))))

(defun fun->key (key fun)
  (unless (assoc key *keyword-fun-alist*)
    (push (list key fun) *keyword-fun-alist*)))

(defun keyword-from-json (json)
  (first (first json)))

(defun execute-json (client json &optional (debug nil))
  (let ((keyword (keyword-from-json json)))
    (when debug
      (format *standard-output* "keyword: ~S~%JSON: ~S~%" keyword json))
    (handler-case (funcall (key->fun keyword) client (rest (first json)))
      (keyword-not-found ()
        (funcall (key->fun :unknown) client (rest (first json)))))))

(defun handle-unknown (client args)
  (declare (ignore client args))
  (print "unknown key"))

(fun->key :unknown 'handle-unknown)

(defun handle-setip (client args)
  "Takes in the tcp port used by the device to listen and the udp port selected by 
the device for sending packets and sets the values of *udp-port* *tcp-port-output*"
  (let* ((tcp (cdr (assoc :tcpinput args)))
         (udp (cdr (assoc :udpport args))))
    (setf (output-tcp-port client) tcp
          (input-udp-port client) udp)))

(fun->key :setip 'handle-setip)

(defun handle-connected (client args)
  "Prints out the valid connection"
  (declare (ignore client args))
  (format *standard-output* "~%connection made~%"))

(fun->key :connected 'handle-connected)

(defun handle-shutdown (client args)
  (declare (ignore client args))
  :shutdown)

(fun->key :shutdown 'handle-shutdown)

(defun handle-click (client args)
  "Handles a click command sent from the client"
  (declare (ignore client args))
  (down))

(fun->key :click 'handle-click)

(defun handle-unclick (client args)
  "Handles an unclick command sent from the client"
  (declare (ignore client args))
  (up))

(fun->key :unclick 'handle-unclick)

(defun handle-start-move (client args)
  "Handles a start-move event sent from client"
  (declare (ignore client args))
  (set-xy-to-mouse-position-now))

(fun->key :start-move 'handle-start-move)

(defun handle-right-click (client args)
  "Handles a right-click event sent from client"
  (declare (ignore client args))
  (right-click))

(fun->key :right-click 'handle-right-click)

(defun handle-text (client args)
  "Handles a text event sent from client"
  (declare (ignore client))
  (when args
    (string-to-fake-events args *display*)))

(fun->key :text 'handle-text)

(defun handle-position (client args)
  "moves the mouse to the x y on *display* and *window* both found in /src/x-control"
  (declare (ignore client))
  (let* ((x (cdr (assoc :x args)));;it will probably be much more complex...
         (y (cdr (assoc :y args))))
    (move-mouse *display* x y)))

(fun->key :position 'handle-position)


