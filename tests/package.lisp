(in-package :cl-user)

(defpackage :cl-http-server-tests
  (:use :cl :cl-http-server :anaphora :5am :drakma))

(in-package :cl-http-server-tests)

(def-suite cl-http-server)

(loop for s in
      '("ASSOC-REF" "CONCAT" "PREG-MATCH" "REPLACE-STR" "SPLIT"
        "HTML" "RANDOM-HEX-STRING")
      do (import (find-symbol s (find-package 'cl-http-server))))
