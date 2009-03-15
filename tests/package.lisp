(in-package :cl-user)

(defpackage :cl-http-server-tests
  (:use :cl :cl-http-server :anaphora :5am :drakma :cl-ppcre))

(in-package :cl-http-server-tests)

(def-suite cl-http-server)

(loop for s in
      '("ASSOC-REF" "->STRING" "PREG-MATCH" "HTML" "RANDOM-HEX-STRING")
      do (import (find-symbol s (find-package 'cl-http-server))))
