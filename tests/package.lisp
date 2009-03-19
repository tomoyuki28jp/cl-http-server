(in-package :cl-user)

(defpackage :cl-http-server-tests
  (:use :cl :cl-http-server :anaphora :5am :drakma :cl-ppcre)
  (:import-from :cl-http-server
                :->string
                :assoc-ref
                :html
                :random-string))

(in-package :cl-http-server-tests)

(def-suite cl-http-server)
