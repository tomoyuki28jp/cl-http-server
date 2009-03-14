(in-package :cl-user)

(defpackage :cl-http-server-asd (:use :cl :asdf))

(in-package :cl-http-server-asd)

(defsystem  :cl-http-server
  :name     "cl-http-server"
  :author   "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
  :licence  "BSD"
  :description "Common Lisp HTTP Server"
  :depends-on  (:anaphora :cl-ppcre :usocket :bordeaux-threads :cl-fad
                :trivial-shell :flexi-streams :rfc2388-binary)
  :components  ((:module "src"
                         :serial  t
                         :components
                         ((:file "package")
                          (:file "specials")
                          (:file "util")
                          (:file "server")))))
