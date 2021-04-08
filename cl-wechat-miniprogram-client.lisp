(in-package :cl-wechat-miniprogram-client)

;;; Common

(defconstant +wechat-api-protocol+ "https")
(defconstant +wechat-api-host+     "api.weixin.qq.com")

;;; WeChat access token API

(defun wechat-api-get-access-token (app-id app-secret
                                    &key
                                      (protocol +wechat-api-protocol+)
                                      (host +wechat-api-host+)
                                      (uri "/cgi-bin/token")
                                      proxy
                                      proxy-basic-authorization)
  "Return access_token\(string\).
  API doc of auth.getAccessToken: https://developers.weixin.qq.com/miniprogram/dev/api-backend/open-api/access-token/auth.getAccessToken.html"
  (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
      (drakma:http-request (format nil "~A://~A~A"
                                   protocol
                                   host
                                   uri)
                           :parameters (list (cons "grant_type" "client_credential")
                                             (cons "appid" app-id)
                                             (cons "secret" app-secret))
                           :proxy proxy
                           :proxy-basic-authorization proxy-basic-authorization)
    (declare (ignore headers uri stream must-close-p status-text))
    (let ((json (jsown:parse (babel:octets-to-string data :encoding :utf-8))))
      (if (eq 200 status-code)
          (values (jsown:val json "access_token")
                  status-code)
          (values nil
                  status-code)))))

(defclass wechat-miniprogram-client ()
  ((protocol
    :initarg :protocol
    :initform +wechat-api-protocol+
    :reader protocol)
   (host
    :initarg :host
    :initform +wechat-api-host+
    :reader host)
   (app-id
    :initarg :app-id
    :initform (error "Must pass a string as value of slot app-id")
    :reader app-id)
   (app-secret
    :initarg :app-secret
    :initform (error "Must pass a string as value of slot app-secret")
    :reader app-secret)
   (access-token
    :initarg :access-token
    :initform nil
    :reader access-token)
   (auto-refresh-access-token-p
    :initarg :auto-refresh-access-token-p
    :initform t
    :reader auto-refresh-access-token-p)
   (proxy
    :initarg :proxy
    :initform nil
    :accessor proxy
    :documentation "e.g. '\(\"127.0.0.1\" 8080\)")
   (proxy-basic-authorization
    :initarg :proxy-basic-authorization
    :initform nil
    :accessor proxy-basic-authorization
    :documentation "e.g. '\(\"username\" \"password\"\)")))

(defmethod initialize-instance :after ((client wechat-miniprogram-client) &rest args)
  (declare (ignore args))
  (flet ((init-access-token ()
           (setf (slot-value client 'access-token)
                 (wechat-api-get-access-token
                  (app-id client)
                  (app-secret client)
                  :proxy (proxy client)
                  :proxy-basic-authorization (proxy-basic-authorization client)))))
    (init-access-token)
    (when (slot-value client 'auto-refresh-access-token-p)
      ;; refresh access-token every hour
      (cron:make-cron-job
       #'init-access-token
       :minute 0
       :hash-key (intern
                  (format nil
                          "~:@(wechat-miniprogram-client-access-token-refresh-cron-job-~A~)"
                          (app-id client))))
      (cron:start-cron))))

(defun make-wechat-miniprogram-client (app-id app-secret
                                       &key
                                         (protocol +wechat-api-protocol+)
                                         (host +wechat-api-host+)
                                         (auto-refresh-access-token-p t)
                                         proxy
                                         proxy-basic-authorization)
  (make-instance 'wechat-miniprogram-client
                 :protocol protocol
                 :host host
                 :app-id app-id
                 :app-secret app-secret
                 :auto-refresh-access-token-p auto-refresh-access-token-p
                 :proxy proxy
                 :proxy-basic-authorization proxy-basic-authorization))

(defmethod print-object ((client wechat-miniprogram-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream ":APP-ID ~S" (app-id client))))

;;; WeChat Mini Programs APIs

(defmethod auth-code2session ((client wechat-miniprogram-client)
                              js-code
                              &key (uri "/sns/jscode2session"))
  "Parameters:
    js-code: js_code from 
  Return session\(jsown\) like this:
  \(:OBJ \
     \(\"session_key\" . \"xxxxx\"\)
     \(\"openid\" . \"yyyyy\"\)
     \(\"unionid\" . \"zzzzz\"\)\)
  Note: unionid will be contained in response only after binding miniprogram to WeChat Dev Platform account.
  API doc of auth.code2Session: https://developers.weixin.qq.com/miniprogram/dev/api-backend/open-api/login/auth.code2Session.html"
  (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
      (drakma:http-request (format nil "~A://~A~A"
                                   (protocol client)
                                   (host client)
                                   uri)
                           :parameters (list (cons "grant_type" "authorization_code")
                                             (cons "appid" (app-id client))
                                             (cons "secret" (app-secret client))
                                             (cons "js_code" js-code))
                           :proxy (proxy client)
                           :proxy-basic-authorization (proxy-basic-authorization client))
    (declare (ignore headers uri stream must-close-p status-text))
    (let ((json (jsown:parse (if (stringp data)
                                 data
                                 (babel:octets-to-string data :encoding :utf-8)))))
      (if (eq 200 status-code)
          (values json status-code)
          (values nil status-code)))))

(defmethod security-img-sec-check ((client wechat-miniprogram-client)
                                   pathname
                                   &key (uri "/wxa/img_sec_check"))
  "Check whether image contain risky content.
  API doc of security.imgSecCheck: https://developers.weixin.qq.com/miniprogram/dev/api-backend/open-api/sec-check/security.imgSecCheck.html"
  (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
      (drakma:http-request (format nil "~A://~A~A?access_token=~A"
                                   (protocol client)
                                   (host client)
                                   uri
                                   (access-token client))
                           :method :post
                           :content-length t
                           :parameters (list (list "media" pathname))
                           :proxy (proxy client)
                           :proxy-basic-authorization (proxy-basic-authorization client))
    (declare (ignore headers uri stream must-close-p status-text))
    (if (eq 200 status-code)
        (let ((response (jsown:parse (babel:octets-to-string data :encoding :utf-8))))
          (if (equal 0 (jsown:val response "errcode"))
              (values t status-code response)
              (values nil status-code response)))
        (values nil status-code))))

(defconstant +search-submit-pages-max-pages-per-request+ 1000)

(defmethod search-submit-pages ((client wechat-miniprogram-client)
                                path-query-alist
                                &key (uri "/wxa/search/wxaapi_submitpages"))
  "Submit new WeChat Mini Program pages to WeChat\(SEO\).
  API doc of search.submitPages: https://developers.weixin.qq.com/miniprogram/dev/api-backend/open-api/search/search.submitPages.html"
  (assert (<= 1
              (length path-query-alist)
              +search-submit-pages-max-pages-per-request+))
  (let ((json-string (jsown:to-json
                      (jsown:new-js
                        ("pages" (loop for (path . query) in path-query-alist
                                    collect (jsown:new-js
                                              ("path" path)
                                              ("query" query))))))))
    (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
        (drakma:http-request (format nil "~A://~A~A?access_token=~A"
                                     (protocol client)
                                     (host client)
                                     uri
                                     (access-token client))
                             :method :post
                             :content-length t
                             :content json-string
                             :proxy (proxy client)
                             :proxy-basic-authorization (proxy-basic-authorization client))
      (declare (ignore headers uri stream must-close-p status-text))
      (if (eq 200 status-code)
          (let ((response (jsown:parse (babel:octets-to-string data :encoding :utf-8))))
            (if (equal 0 (jsown:val response "errcode"))
                (values t   status-code response)
                (values nil status-code response)))
          (values nil status-code (babel:octets-to-string data :encoding :utf-8))))))

(defmethod wxacode-get-unlimited ((client wechat-miniprogram-client)
                                  scene
                                  page
                                  &key
                                    (width 300)
                                    (uri "/wxa/getwxacodeunlimit"))
  "Return data\(octets\) of image of WXACode\(like QRCode\) in JPEG format.
  Parameters:
    scene: 32 character max
    page: published WeChat Mini Program page begin with page/
  API doc of wxacode.getUnlimited: https://developers.weixin.qq.com/miniprogram/dev/api-backend/open-api/qr-code/wxacode.getUnlimited.html"
  (multiple-value-bind (data status-code headers uri stream must-close-p status-text)
      (drakma:http-request (format nil "~A://~A~A?access_token=~A"
                                   (protocol client)
                                   (host client)
                                   uri
                                   (access-token client))
                           :method :post
                           :content (jsown:to-json
                                     (jsown:new-js
                                       ("scene" scene)
                                       ("page" page)
                                       ("width" width)))
                           :proxy (proxy client)
                           :proxy-basic-authorization (proxy-basic-authorization client))
    (declare (ignore headers uri stream must-close-p status-text))
    (if (eq 200 status-code)
        data
        (error "ERROR when get wxa-unlimited with sene ~A: ~A~%"
               scene
               (babel:octets-to-string data :encoding :utf-8)))))

(defmethod wxacode-get-unlimited-to-file ((client wechat-miniprogram-client)
                                          scene
                                          page
                                          pathname
                                          &key
                                            (width 300))
  (let ((image-octets (wxacode-get-unlimited client
                                             scene
                                             page
                                             :width width)))
    (when (> (length image-octets)
             2000)
      (ensure-directories-exist (directory-namestring pathname))
      (with-open-file (stream pathname
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence image-octets stream))
      pathname)))

(defun decrypt-private-user-info (encrypted-private-user-info session-key iv)
  "Decrypt encryptedData from Weixin mini-program API wx.getUserInfo or <button open-type=\"getPhoneNumber\">Auth Phone Number</button> in WeChat Mini Program.
  Parameters:
    encrypted-data: string\(base64 string\), encryptedData of response of API wx.getUserInfo or <button open-type=\"getPhoneNumber\">Auth Phone Number</button> in WeChat Mini Program
    session-key: string, session_key of response of API auth.code2session, as encoding-aes-key
    iv: string\(base64 string\), iv of response of API auth.code2session
  Note: API auth.code2Session require parameter js_code\(code of response of API wx.login\)
  API doc of auth.code2Session: https://developers.weixin.qq.com/miniprogram/dev/api-backend/open-api/login/auth.code2Session.html
  API doc of signature, verification, encryption, and decryption of user data: https://developers.weixin.qq.com/miniprogram/en/dev/framework/open-ability/signature.html#Encryption-Algorithm-for-Encrypted-Data"
  (let* ((decrypt-cipher (ironclad:make-cipher 'ironclad:aes
                                               :key (base64:base64-string-to-usb8-array
                                                     session-key)
                                               :mode 'ironclad:cbc
                                               :initialization-vector
                                               ;; iv 
                                               (base64:base64-string-to-usb8-array iv)))
         (encrypted-private-user-info (base64:base64-string-to-usb8-array
                                       encrypted-private-user-info)))
    ;; AES decrypt
    (ironclad:decrypt-in-place decrypt-cipher encrypted-private-user-info)
    (let* ((decrypted-private-user-info encrypted-private-user-info)
           ;; length of encrypted message and decrypted message is same in AES
           (private-user-info-byte-vector-length (length decrypted-private-user-info))
           (padding-length (elt decrypted-private-user-info
                                (1- private-user-info-byte-vector-length))))
      ;; decrypted private user info
      (babel:octets-to-string (subseq decrypted-private-user-info
                                      0
                                      ;; ignore padding bytes
                                      (- private-user-info-byte-vector-length
                                         padding-length))
                              :encoding :utf-8))))
