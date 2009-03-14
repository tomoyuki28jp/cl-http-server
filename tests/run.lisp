(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-http-server-tests))

(in-package :cl-http-server-tests)

(defmacro run-test (test)
  `(progn
     (setf *srv* (start-server (make-server :public-dir *test-public-dir*
                                            :port 8080 :timeout-sec 3)))
     (unwind-protect
          (5am:run! ,test)
       (progn
         (stop-server *srv*)))))

; run each test
;(run-test 'preg-match)

; run all tests
;(run-test 'cl-http-server)
