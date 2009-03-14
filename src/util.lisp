(in-package :cl-http-server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun concat (&rest args)
    (with-output-to-string (s)
      (dolist (a args)
        (when a
          (princ a s))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun join (joiner lst)
    (format nil (concat "~{~A~^" joiner "~}") lst)))

(defun assoc-ref (item alist &rest args)
  (awhen (apply #'assoc item alist args)
       (cdr it)))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (symbol-macrolet
           ,(mapcar #'(lambda (f)
                        `(,f (,(intern (concat name "-" f)) ,gs)))
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

(defun make-keyword (x)
  (if (keywordp x)
      x
      (let ((str (if (stringp x) x (concat x))))
        (intern (string-upcase str) :keyword))))

(defun random-hex-string (length)
  (format nil "~v,'0x"
          length (random (expt 16 length) *the-random-state*)))

(defun preg-match (regexp str)
  (cl-ppcre:scan-to-strings
   (cl-ppcre:create-scanner regexp) str))

(defun position-str (delimiter str &key (start 0))
  (when (plusp start)
    (setq str (subseq str start (length str))))
  (let ((del-len  (length delimiter))
        (str-len  (length str))
        (1st-char (char delimiter 0)))
    (loop for i = 0 then (1+ j)
          as  j = (position 1st-char str :start i)
          when (and j
                    (<= (+ j del-len) str-len)
                    (string= delimiter (subseq str j (+ j del-len))))
            return (+ j start)
          end
          while j)))

(defgeneric split (delimiter str)
  (:documentation "Split a string by a delimiter"))

(defmethod split ((delimiter character) (str string))
  (loop for i = 0 then (1+ j)
        as  j = (position delimiter str :start i)
        collect (subseq str i j)
        while j))

(defmethod split ((delimiter string) (str string))
  (if (or (string= delimiter "")
          (string= delimiter str))
    (list str)
    (let ((del-len (length delimiter)))
      (loop for i = 0 then (+ j del-len)
            as  j = (position-str delimiter str :start i)
            collect (subseq str i j)
            while j))))

(defun replace-str (search replace str)
  (if (null search)
    str
    (let ((s (split search str)))
      (if (plusp (length s))
        (join replace s)
        str))))

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

(defun uniq-file-name (dir &optional (name-length 10))
  (dotimes (x 5)
    (let ((file (concat dir (random-hex-string name-length))))
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
