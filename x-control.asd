;;;;package definition
(in-package #:asdf-user)
(asdf:defsystem "x-control"
  :description "Backend for my X-Control android app"
  :name "X-control"
  :author "K1D77A"
  :version "1"
  :license "zlib"
  :depends-on (#:clx
               #:usocket
               #:cl-json
               #:safe-queue
               #:bordeaux-threads)
                                        ; #:cl-configurator)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "classes")
               (:file "x-control") 
               (:file "commands")
               (:file "networking/tcp-handler")
               (:file "networking/udp-handler")
               (:file "server")))
