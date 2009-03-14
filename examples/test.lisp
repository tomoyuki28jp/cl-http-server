(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-http-server))

(in-package :cl-user)
(defpackage :cl-http-server-test (:use :cl :cl-http-server))
(in-package :cl-http-server-test)

; http://localhost:8080/test
(defpage test ()
  (html :title "Test page title"
        :body  "Hello, world!"))

; http://localhost:8080/get?get1=hello&get2=world
(defpage get (:get get1 get2)
  (html :body (concatenate 'string "get1: " get1 " get2: " get2)))

(defparameter *srv* (start-server))
;(stop-server *srv*)
