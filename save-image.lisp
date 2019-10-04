(defun load-up ()
  (asdf:load-asd "/home/josh/documents/lisp/programs/x-control/x-control-package.asd")
  (asdf:load-system "X-control"))
;(load-up)
  
(defun save ()
  (save-lisp-and-die "server" :toplevel 'x-control:setup-x-control		     
			      :executable t :compression 9))
