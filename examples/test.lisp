; To run:
; 1. compile and load this file
; 2. go to http://localhost:8080/

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-http-server))

(in-package :cl-user)
(defpackage :cl-http-server-test (:use :cl :cl-http-server))
(in-package :cl-http-server-test)

;(defparameter *srv* (start-server))
;(stop-server *srv*)
