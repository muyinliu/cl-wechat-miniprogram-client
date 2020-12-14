(defsystem "cl-wechat-miniprogram-client"
  :name "cl-wechat-miniprogram-client"
  :description "WeChat Mini Program client for server API in Common Lisp."
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "ISC"
  :depends-on ("drakma"
               "jsown"
               "cl-cron"
               "ironclad")
  :serial t
  :components ((:file "packages")
               (:file "cl-wechat-miniprogram-client")))
