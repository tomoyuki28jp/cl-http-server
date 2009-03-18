(in-package :cl-http-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ->string (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
        (princ (or a "") s)))))

(defun ->keyword (x)
  (if (keywordp x)
      x
      (let ((str (if (stringp x) x (->string x))))
        (intern (string-upcase str) :keyword))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun join (joiner &rest args)
    (format nil (->string "~{~A~^" joiner "~}")
            (remove nil args))))

(defun assoc-ref (item alist &rest args)
  (awhen (apply #'assoc item alist args)
       (cdr it)))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (symbol-macrolet
           ,(mapcar #'(lambda (f)
                        `(,f (,(intern (->string name "-" f)) ,gs)))
                    fields)
         ,@body))))

(defmacro with-flexi-stream ((stream external-format) &rest body)
  `(let ((,stream (make-flexi-stream
                   ,stream :external-format ,external-format)))
     ,@body))

(defun ensure-file-exist (file)
  (handler-case
      (with-open-file
          (stream file :direction :output :if-exists :append
                  :if-does-not-exist :create)
        t)
    (error () nil)))

(defun iso-time (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun is-readable (file)
  (let ((file (namestring file)))
    (unless (not (probe-file file))
      (handler-case
          (with-open-file (stream file)
            t)
        (error () nil)))))

(defun qw (str)
  (format nil "\"~A\"" (or str "")))

(defun random-string (&optional (n 10) (base 16))
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defun preg-match (regexp str)
  (scan-to-strings (create-scanner regexp) str))

(defun uri-encode (str)
  (with-output-to-string (s)
    (loop for c across str
          for i from 0
          do (if (or (char<= #\0 c #\9)
                     (char<= #\a c #\z)
                     (char<= #\A c #\Z)
                     (find c "'.-*()_" :test #'char=))
                 (write-char c s)
                 (loop for o across
                       (string-to-octets str :start i :end (1+ i)
                                         :external-format :utf-8)
                       do (format s "%~2,'0x" o))))))

(defun uri-decode (str)
  (let* ((len (length str))
         (vec (make-array
               len :element-type '(unsigned-byte 8) :fill-pointer 0))
         (idx 0))
    (flet ((vec-push (x) (vector-push x vec)))
      (do ()
          ((not (< idx len)))
        (let ((c (char str idx)))
          (cond ((char= c #\%)
                 (vec-push
                  (parse-integer
                   str :start (1+ idx) :end (+ 3 idx) :radix 16))
                 (incf idx 2))
                ((char= c #\+) (vec-push 32))
                (t (vec-push
                    (elt (string-to-octets
                          (string c) :external-format :utf-8) 0)))))
        (incf idx)))
    (octets-to-string vec :external-format :utf-8)))

(defun uniq-file-name (dir &optional (length 10))
  (dotimes (x 5)
    (let ((file (->string dir (random-string length))))
      (unless (probe-file file)
        (return-from uniq-file-name file)))))

(defun html (&key (title "Default title") (body  "Default body"))
  (format *http-char-stream*
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN
http://www.w3.org/TR/html4/loose.dtd\">
<HTML LANG=\"en\">
<HEAD>
<META HTTP-EQUIV=\"content-type\" CONTENT=\"text/html; charset=utf-8\">
<TITLE>~A</TITLE>
</HEAD>
<BODY>
~A
</BODY>
</HTML>" title body))
