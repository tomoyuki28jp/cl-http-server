(in-package :cl-user)

(defpackage :cl-http-server-tests
  (:use :cl :cl-http-server :my-util :5am :drakma)
  (:import-from :cl-http-server :html :random-string))

(in-package :cl-http-server-tests)

(def-suite cl-http-server)
