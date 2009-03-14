(in-package :cl-user)

(defpackage :cl-http-server-tests-asd (:use :cl :asdf))

(in-package :cl-http-server-tests-asd)

(defsystem  :cl-http-server-tests
  :name     "cl-http-server-tests"
  :author   "Tomoyuki Matsumoto <tomoyuki28jp@no-spam@yahoo.co.jp>"
  :licence  "BSD"
  :description "tests for cl-http-server"
  :depends-on  (:cl-http-server :anaphora :fiveam :drakma)
  :components  ((:module "tests"
                         :serial  t
                         :components ((:file "package")
                                      (:file "specials")
                                      (:file "util")
                                      (:file "server")))))
