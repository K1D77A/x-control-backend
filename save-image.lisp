(defun load-up ()
  (ql:quickload :x-control))
;(load-up)
  
(defun save ()
  (save-lisp-and-die "server" :toplevel 'x-control:setup-x-control		     
                              :executable t :compression 9))


