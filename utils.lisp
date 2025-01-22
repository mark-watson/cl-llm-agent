(in-package :cl-llm-agent-utils)

(defun http-post-json (url data &optional headers)
  "Makes an HTTP POST request with JSON data and returns parsed JSON response."
  (drakma:http-request url
                       :method :post
                       :content-type "application/json"
                       :content (cl-json:encode-json-to-string data)
                       :additional-headers headers
                       :external-format-out :utf-8
                       :external-format-in :utf-8))

(defun parse-json (json-string)
  "Parses a JSON string into a Lisp data structure."
  (cl-json:decode-json-from-string json-string))

