(in-package :cl-http-server-tests)

(test concat
  (is (string= (concat "a" "i" "u" "e" "o") "aiueo"))
  (is (string= (concat nil "a" "i" "u" "e" "o") "aiueo"))
  (is (string= (concat nil "a" "i" "u" "e" "o" nil) "aiueo"))
  (is (string= (concat nil "a" nil "i" nil "u" nil "e" nil "o" nil) "aiueo")))

(test join
  (is (string= (join "|" '("a" "i" "u")) "a|i|u"))
  (is (string= (join ""  '("a" "i" "u")) "aiu")))

(test assoc-ref
  (let ((lst '((200 . "number"))))
    (is (assoc-ref 200 lst)) "number")
  (let ((lst '(("200" . "string"))))
    (is (assoc-ref "200" lst :test 'equal)) "string"))

(test with-struct
  (defstruct str
    (s1)
    (s2))
  (let ((str (make-str :s1 "s1" :s2 "s2")))
    (is (string= (str-s1 str) "s1"))
    (is (string= (str-s2 str) "s2")))
  (let ((str (make-str)))
    (with-struct (str s1 s2) str
      (setf s1 :s1)
      (setf s2 :s2)
      (is (eq (str-s1 str) :s1))
      (is (eq (str-s2 str) :s2)))))

(test with-flexi-stream
  (with-open-file (in "/tmp/flexi-test" :if-does-not-exist :create)
    (with-flexi-stream (in :utf-8)
      (is (typep (flexi-streams:flexi-stream-external-format in)
                 'FLEXI-STREAMS::FLEXI-UTF-8-FORMAT)))
    (with-flexi-stream (in :iso-8859-1)
      (is (typep (flexi-streams:flexi-stream-external-format in)
                 'FLEXI-STREAMS::FLEXI-LATIN-1-FORMAT)))))

(test ensure-file-exist
  (loop for i from 1 to 3
        as f = (mkstr "/tmp/web4r.util.test" i)
        do (progn
             (when (is-readable f)
               (trivial-shell:shell-command (mkstr "rm -f " f)))
             (when (is-readable f)
               (error (format nil "file ~A already exists" f)))
             (ensure-file-exist f)
             (is (eq t (is-readable f)))
             (trivial-shell:shell-command (mkstr "rm -f " f)))))

(test iso-time
  (is (equal (iso-time 3161620249) "2000-03-10 04:50:49"))
  (is (equal (iso-time 3361620000) "2006-07-12 00:20:00"))
  (is (equal (iso-time 3443620249) "2009-02-15 02:10:49")))

(test is-readable
  (is (eq t (is-readable "/etc/"))))

(test qw
  (is (string= (qw "str") "\"str\""))
  (is (string= (qw nil) "\"\""))
  (is (string= (qw "\"") "\"\"\"")))

(test make-keyword
  (is (eq (make-keyword "k1") :k1))
  (is (eq (make-keyword 'k2)  :k2))
  (is (eq (make-keyword :k3)  :k3)))

(test random-hex-string
 (let ((lst (loop for i from 1 to 10
                 collect (random-hex-string 32))))
   (loop for r in lst
         as  i from 0
         do (progn
              (is (string= (preg-match "^[0-9A-F]+$" r) r))
              (is (null (member r (remseq lst i (1+ i)))))))))

(test preg-match
  (let ((n "12345"))
    (is (string= (preg-match "[0-9]" n) "1")))
  (let ((n "12345"))
    (is (string= (preg-match "^[0-9]+$" n) n)))
  (let ((tel "408-644-6198"))
    (is (string= (preg-match "^\\d{3}-\\d{3}-\\d{4}$" tel) tel)))
  (let ((s "asdfgjkl"))
    (is (string= (preg-match "^[a-z]+$" s) s)))
  (let ((s "ADLBDB"))
    (is (string= (preg-match "^[A-Z]+$" s) s))))

(test uri-encode
  (is (string= (uri-encode "ニュース速報")
               "%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B9%E9%80%9F%E5%A0%B1"))
  (is (string= (uri-encode "'.-*()_") "'.-*()_"))
  (is (string= (uri-encode "UTF-8") "UTF-8"))
  (is (string= (uri-encode " ") "%20")))

(test uri-decode
  (is (string= (uri-decode
                "%E3%83%8B%E3%83%A5%E3%83%BC%E3%82%B9%E9%80%9F%E5%A0%B1")
               "ニュース速報"))
  (is (string= (uri-decode "'.-*()_") "'.-*()_"))
  (is (string= (uri-decode "UTF-8") "UTF-8"))
  (is (string= (uri-decode "%20") " ")))

(test uniq-file-name
  (let ((dir "/tmp/web4r-test/")
        (times 10))
    (when (probe-file dir)
      (cl-fad:delete-directory-and-files dir))
    (ensure-directories-exist dir :verbose nil)
    (let (files)
      (dotimes (x times)
        (let ((file (uniq-file-name dir)))
          (push file files)
          (ensure-file-exist file)))
      (is (eq (length (cl-fad:list-directory dir)) times))
      (loop for f in files
            do (is (probe-file f)))
      (cl-fad:delete-directory-and-files dir))))
