(in-package :cl-http-server-tests)

(defvar *test-dir*
  (cl-http-server::awhen (load-time-value #.*compile-file-pathname*)
    (directory-namestring cl-http-server::it)))

(defvar *test-public-dir*
  (namestring (merge-pathnames "public/" *test-dir*)))

(defvar *nl* (format nil "~%"))

(defvar *srv* nil)
