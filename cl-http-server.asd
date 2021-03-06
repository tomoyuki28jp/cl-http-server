(in-package :cl-user)

(defpackage :cl-http-server-asd (:use :cl :asdf))

(in-package :cl-http-server-asd)

(defsystem  :cl-http-server
  :version "0.0.3"
  :name     "cl-http-server"
  :author   "Tomoyuki Matsumoto <tomoyuki28jp@gmail.com>"
  :licence  "BSD"
  :description "Common Lisp HTTP Server"
  :depends-on  (:my-util :usocket :bordeaux-threads :cl-fad
                :trivial-shell :flexi-streams :rfc2388-binary)
  :components  ((:module "src"
                         :serial  t
                         :components
                         ((:file "package")
                          (:file "specials")
                          (:file "util")
                          (:file "server")))))
