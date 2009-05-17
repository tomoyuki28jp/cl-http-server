(in-package :cl-user)

(defpackage :cl-http-server
  (:use :cl :my-util :usocket :cl-fad :flexi-streams)
  (:export :html
           :*server*
           :*request*
           :*response*
           :*http-stream*
           :*http-char-stream*
           :*http-binary-stream*
           :*sid*p
           :get-route
           :add-route
           :rem-route
           :destroy-session
           :route-static
           :route-regex
           :make-server
           :validate-server
           :start-server
           :stop-server
           :server-is-running-p
           :access-log
           :error-log
           :debug-log
           :get-cookies
           :get-cookie
           :set-cookie
           :get-cookie-sid
           :get-session
           :rem-session
           :set-session
           :session-file
           :session-name
           :generate-sid
           :valid-sid-p
           :add-header
           :status-page
           :add-hook
           :get-page
           :set-page
           :serve-file
           :public-file-p
           :image-type
           :host-uri
           :page-uri
           :page-lambda
           :defpage
           :default-page
           :page
           :uri-path
           :get-params
           :get-param
           :post-params
           :post-param
           :header-fields
           :header-field
           :redirect
           :exit
           :public-dir
           :req-uri
           :get-file-data
           :file-name
           :file-type
           :file-tmp-name
           :file-size
           :file-save-name
           ))

(asdf-version<= :my-util "0.0.4")
