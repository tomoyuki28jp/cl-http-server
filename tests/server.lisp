(in-package :cl-http-server-tests)
(in-suite cl-http-server)

(defun p (obj)
  (when obj
    (princ obj *http-char-stream*))
  nil)

(defun p/ (&rest args)
  (dolist (a args)
    (p a)))

(defun string=* (str1 str2)
  (string= (replace-str *nl* "" str1)
           (replace-str *nl* "" str2)))

(defmacro shtml= (shtml html)
  `(let ((*http-char-stream* (make-string-output-stream)))
     ,shtml
     (string=* (get-output-stream-string *http-char-stream*)
               ,html)))

(defun file-content= (file &optional content)
  (unless content
    (setf content (http-request (concat "http://localhost:8080/" file))))
  (let ((e (if (stringp content) 'base-char '(unsigned-byte 8))))
    (with-open-file (in (merge-pathnames file *test-public-dir*) :element-type e)
      (let* ((length (file-length in))
             (array  (make-array length :element-type e)))
        (read-sequence array in)
        (equalp array content)))))

(defun upload-file= (file)
  (file-content= file
                 (http-request "http://localhost:8080/upload-test"
                               :method :post
                               :content-length t
                               :parameters
                               (list (list "foo" (merge-pathnames
                                                  file *test-public-dir*))))))

(defun status-code= (uri status-code)
  (multiple-value-bind (body status-code* headers uri stream close reason-phrase)
      (http-request (concat "http://localhost:8080/" uri))
    (declare (ignore body headers uri stream close reason-phrase))
    (= status-code* status-code)))

(defun content-type= (uri content-type)
  (multiple-value-bind (body status-code headers uri stream close reason-phrase)
      (http-request (concat "http://localhost:8080/" uri))
    (declare (ignore body status-code uri stream close reason-phrase))
    (equalp content-type
            (awhen (cl-http-server::assoc-ref :content-type headers)
              (car (split "; " it))))))

(defun test-rewrite-rule (uri)
  (let ((match (preg-match "(\.ico|\.gif|\.jpeg|\.jpg|\.png|)$" uri)))
    (when (not (string= match ""))
      "/test.html")))

(defun matched-page ()
  (html :body "matched"))

(defun defpage-test1 ()
  (html :body "defpage-test1"))

; Drakma has a bug which drakma don't send cookies when a request uri is 
; a root directory even when cookie path is set to "/". This function is 
; to avoid that bug. The bug has been alread fixed in dev repo on 2009/02/25.
; http://bknr.net/trac/changeset/4322
(defun http-request* (uri &rest args)
  (apply #'http-request
         (append (list (replace-str "http://localhost:8080/"
                                    "http://localhost:8080//" uri))
                 args)))

(test static-file
  (is (file-content= "test"))
  (is (file-content= "test.css"))
  (is (file-content= "test.gif"))
  (is (file-content= "test.html"))
  (is (file-content= "test.ico"))
  (is (file-content= "test.jpeg"))
  (is (file-content= "test.js"))
  (is (file-content= "test.png"))
  (is (file-content= "test.txt"))
  (is (file-content= "test.zip")))

(defvar *test-tmp-files* nil)
(test file-upload
  (setf *test-tmp-files* nil)
  (defpage upload-test ()
    (awhen (post-param "foo")
      (awhen (assoc-ref "tmp-name" it :test #'equal)
        (push it *test-tmp-files*)
        (serve-file it :public-file-only nil))))
  (is (upload-file= "test"))
  (is (upload-file= "test.css"))
  (is (upload-file= "test.gif"))
  (is (upload-file= "test.html"))
  (is (upload-file= "test.ico"))
  (is (upload-file= "test.jpeg"))
  (is (upload-file= "test.js"))
  (is (upload-file= "test.png"))
  (is (upload-file= "test.txt"))
  (is (upload-file= "test.zip"))
  (loop for f in *test-tmp-files*
        do (is-false (probe-file f))))

(test get-params
  (defpage get-params-test () (p/ (get-params)))
  (is-true (shtml= (p/ '(("k1" . "v1")("k2" . "v2")("k3" . "v3")))
                   (http-request
                    "http://localhost:8080/get-params-test?k1=v1&k2=v2&k3=v3"))))

(test get-param
  (defpage get-param-test () (p/ (get-param "k1")))
  (is-true (shtml= (p/ "v1")
                   (http-request
                    "http://localhost:8080/get-param-test?k1=v1"))))

(test post-params
  (defpage post-params-test () (p/ (post-params)))
  (is-true (shtml= (p/ '(("k1" . "v1")("k2" . "v2")))
                   (http-request
                    "http://localhost:8080/post-params-test"
                    :method :post :form-data t
                    :parameters '(("k1" . "v1") ("k2" . "v2")))))
                                        ; (multipart/form-data)
  (is-true (shtml= (p/ '(("k1" . "v1")("k2" . "v2")))
                   (http-request
                    "http://localhost:8080/post-params-test"
                    :method :post
                    :parameters '(("k1" . "v1") ("k2" . "v2"))))))

(test post-param
  (defpage post-param-test () (p/ (post-param "k1")))
  (is-true (shtml= (p/ "v1")
                   (http-request
                    "http://localhost:8080/post-param-test"
                    :method :post :form-data t :parameters '(("k1" . "v1"))))))

(test defpage
  (defpage defpage-test1 () (defpage-test1))
  (is-true (shtml= (defpage-test1)
                   (http-request "http://localhost:8080/defpage-test1")))
  (defpage defpage-test2 (p1 p2) (p/ p1 p2))
  (is-true (shtml= (p/ "pv1" "pv2")
                   (http-request "http://localhost:8080/defpage-test2/pv1/pv2/")))
  (defpage defpage-test3 (:get p1 p2) (p/ p1 p2))
  (is-true (shtml= (p/ "pv1" "pv2")
                   (http-request
                    "http://localhost:8080/defpage-test3?p1=pv1&p2=pv2")))
  (defpage defpage-test4 (:post p1 p2) (p/ p1 p2))
  (is-true (shtml= (p/ "pv1" "pv2")
                   (http-request
                    "http://localhost:8080/defpage-test4"
                    :method :post :form-data t
                    :parameters '(("p1" . "pv1") ("p2" . "pv2"))))))

;(test page-lambda
;  (defpage page-lambda-test1 ()
;    (form/cont/ (page-lambda (:post foo) (p/ foo))
;      (input-text/ "foo")))
;  (let ((c (make-instance 'cookie-jar)))
;    (multiple-value-bind (match regs)
;        (preg-match "NAME=\"cid\" VALUE=\"(.+)\""
;                    (http-request
;                     "http://localhost:8080/page-lambda-test1"
;                     :cookie-jar c))
;      (is-true (shtml= (p/ "ok")
;                       (http-request  "http://localhost:8080//"
;                                      :method :post :form-data t
;                                      :parameters
;                                      (list (cons "cid" (elt regs 0))
;                                            (cons "foo" "ok"))
;                                      :cookie-jar c))))))

(test page
  (is-true (shtml= (cl-http-server::%status-page 400)
                   (http-request "http://localhost:8080/../../../../../../etc/passwd")))
  (is-true (shtml= (cl-http-server::%status-page 404)
                   (http-request "http://localhost:8080/nopage")))
  (defpage page-test1 () (matched-page))
  (defpage page-test2 () (page 'page-test1))
  (is-true (shtml= (matched-page)
                   (http-request "http://localhost:8080/page-test2")))
  (is-true (shtml= (default-page)
                   (http-request "http://localhost:8080/"))))

(test status-code
  (is (status-code= "test"           200))
  (is (status-code= "test.css"       200))
  (is (status-code= "test.gif"       200))
  (is (status-code= "test.html"      200))
  (is (status-code= "test.ico"       200))
  (is (status-code= "test.jpeg"      200))
  (is (status-code= "test.js"        200))
  (is (status-code= "test.png"       200))
  (is (status-code= "test.txt"       200))
  (is (status-code= "test.zip"       200))
  (is (status-code= "../../../../../../etc/passwd" 400))
  (is (status-code= "nopage"         404))
  (is (status-code= "defpage-test1"  200))
  (is (status-code= ""               200)))

(test content-type
  (is (content-type= "test"          nil))
  (is (content-type= "test.css"      "text/css" ))
  (is (content-type= "test.gif"      "image/gif"))
  (is (content-type= "test.html"     "text/html"))
  (is (content-type= "test.ico"      "image/x-icon"))
  (is (content-type= "test.jpeg"     "image/jpeg"))
  (is (content-type= "test.js"       "application/x-javascript"))
  (is (content-type= "test.png"      "image/png"))
  (is (content-type= "test.txt"      nil))
  (is (content-type= "test.zip"      "application/zip"))
  (is (content-type= "defpage-test1" "text/html"))
  (is (content-type= ""              "text/html")))

(test rewrite-rule
  (setf (cl-http-server::server-rewrite-rule *srv*) 'test-rewrite-rule)
  (is (file-content= "test.html"
                     (http-request "http://localhost:8080/test.gif")))
  (is (file-content= "test.html"
                     (http-request "http://localhost:8080/test.png")))
  (is (file-content= "test.html"))
  (is (file-content= "test.js"))
  (is (file-content= "test.css"))
  (setf (cl-http-server::server-rewrite-rule *srv*) nil))

(test static-route
  (add-route *srv* :static "/nopage" 'matched-page)
  (is-true (shtml= (matched-page)
                   (http-request "http://localhost:8080/nopage")))
  (rem-route *srv* :static "/nopage")
  (is-true (shtml= (cl-http-server::%status-page 404)
                   (http-request "http://localhost:8080/nopage"))))

(test regex-route
  (is-true (shtml= (cl-http-server::%status-page 404)
                   (http-request "http://localhost:8080/matched1")))
  (add-route *srv* :regex "^/matched" 'matched-page)
  (is-true (shtml= (matched-page)
                   (http-request "http://localhost:8080/matched1")))
  (is-true (shtml= (cl-http-server::%status-page 404)
                   (http-request "http://localhost:8080/no/matched1")))
  (is-true (shtml= (matched-page)
                   (http-request "http://localhost:8080/matched2")))
  (rem-route *srv* :regex "^/matched")
  (is-true (shtml= (cl-http-server::%status-page 404)
                   (http-request "http://localhost:8080/matched1"))))

(test cookie
  (let ((r (random-hex-string 10)))
    (defpage cookie-set-test () (set-cookie "cookie-set-test1" r))
    (defpage cookie-get-test () (p/ (get-cookie "cookie-set-test1")))
    (let ((cookie (make-instance 'cookie-jar)))
      (http-request "http://localhost:8080/cookie-set-test" :cookie-jar cookie)
      (is-true (shtml= (p/ r)
                       (http-request "http://localhost:8080/cookie-get-test"
                                     :cookie-jar cookie))))))

(test session
  (let ((r (random-hex-string 10)))
    (defpage session-set-test () (set-session "session-set-test1" r))
    (defpage session-get-test () (p/ (get-session "session-set-test1")))
    (let ((cookie (make-instance 'cookie-jar)))
      (http-request "http://localhost:8080/session-set-test" :cookie-jar cookie)
      (is-true (shtml= (p/ r)
                       (http-request "http://localhost:8080/session-get-test"
                                     :cookie-jar cookie))))))

(test host-uri
  (defpage host-uri-test () (p (host-uri)))
  (is (equal "http://localhost:8080/"
             (http-request "http://localhost:8080/host-uri-test"))))

(test header-field
  (defpage header-field-test () (p (header-field "Host")))
  (is (equal "localhost:8080"
             (http-request "http://localhost:8080/header-field-test"))))

(test redirect
  (defpage redirect-test1 ()
    (redirect (page-uri 'redirect-test2)))
  (defpage redirect-test2 () (p "ok"))
  (is (equal "ok" (http-request "http://localhost:8080/redirect-test2"))))

(test file-data
  (defpage file-data-test1 ()
    (p (concat (file-name "foo") "-"
               (file-type "foo") "-"
               (file-size "foo"))))
  (is (equal "test.gif-image/gif-1841"
             (http-request "http://localhost:8080/file-data-test1"
                           :method :post :content-length t
                           :parameters
                           (list (list "foo"
                                       (merge-pathnames "test.gif"
                                                        *test-public-dir*)
                                       :content-type "image/gif"
                                       :filename "test.gif")))))
  (is (equal "test.png-image/gif-2497"
             (http-request "http://localhost:8080/file-data-test1"
                           :method :post :content-length t
                           :parameters
                           (list (list "foo"
                                       (merge-pathnames "test.png"
                                                        *test-public-dir*)
                                       :content-type "image/gif"
                                       :filename "test.png"))))))
