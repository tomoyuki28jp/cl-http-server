(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-http-server-tests))

(in-package :cl-http-server-tests)

(let ((*srv* (start-server
              (make-server :public-dir *test-public-dir* :port 8080))))
  (unwind-protect (5am:run! 'cl-http-server)
    (stop-server *srv*)))
