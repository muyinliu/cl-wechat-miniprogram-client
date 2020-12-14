(in-package :cl-user)

(defpackage cl-wechat-miniprogram-client
  (:use :cl)
  (:nicknames :wechat-miniprogram-client :wxa-client :wxa)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export
   ;; constants
   #:+wechat-api-protocol+
   #:+wechat-api-host+
   #:+search-submit-pages-max-pages-per-request+
   ;; classes
   #:wechat-miniprogram-client
   ;; accessors
   #:protocol
   #:host
   #:app-id
   #:app-secret
   #:access-token
   #:auto-refresh-access-token-p
   ;; functions
   #:wechat-api-get-access-token
   #:make-wechat-miniprogram-client
   #:auth-code2session
   #:security-img-sec-check
   #:search-submit-pages
   #:wxacode-get-unlimited
   #:decrypt-private-user-info
   ;; utility functions
   #:wxacode-get-unlimited-to-file))

(in-package :cl-wechat-miniprogram-client)

#+sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
