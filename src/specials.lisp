(in-package :cl-http-server)

(defvar *nl* (format nil "~%"))

(defvar *crlf*
  (format nil "~C~C" #\Return #\Linefeed))

(defvar *server*  nil
  "Instance of the server structure")

(defvar *request* nil
  "Instance of the request structure")

(defvar *response* nil
  "Instance of the response structure")

(defvar *http-stream* *standard-output*
  "HTTP stream")

(defvar *http-char-stream* *standard-output*)

(defvar *http-binary-stream* *standard-output*
  "HTTP binary stream")

(defvar *pages* (make-hash-table)
  "Page hash table")

(defvar *sid* nil
  "Session ID")

(defvar *hooks* (make-hash-table))

(defparameter *the-random-state*
  (make-random-state t))
