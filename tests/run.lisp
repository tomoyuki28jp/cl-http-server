(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-http-server-tests))

(in-package :cl-http-server-tests)

(progn
  (let ((srv (make-server :public-dir *test-public-dir* :port 8080)))
    (setf *srv* (start-server srv))
    (unwind-protect
         (5am:run! 'cl-http-server)
      (stop-server *srv*))))
