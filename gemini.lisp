(defpackage :cl-llm-agent-gemini
  (:use :cl)
  (:import-from :babel :octets-to-string)
  (:export :*gemini-api-key*
           :*gemini-base-url*
           :gemini-generate-content))

(in-package :cl-llm-agent-gemini)

;; Load dependencies
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:babel :jonathan :dexador)))

(defvar *gemini-api-key* (uiop:getenv "GOOGLE_API_KEY")
  "Your Google Gemini API key. Set this before using Gemini functions.")

(defvar
  *gemini-base-url*
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
  "Base URL for Gemini API.")

;; Define the conversion function using the imported babel function directly
(defun convert-bytes-to-string (bytes)
  (octets-to-string bytes :encoding :utf-8))

(defun http-post-json (url data)
  "Make a POST request with JSON data."
  (dex:post url
            :headers '(("Content-Type" . "application/json"))
            :content (jonathan:to-json data)))

(defun parse-json (json-string)
  "Parse JSON string to Lisp data structure."
  (jonathan:parse json-string))

(defun gemini-generate-content (prompt &key (api-key *gemini-api-key*))
  "Calls the Gemini API to generate content based on the prompt."
  (unless api-key
    (error "Gemini API key is not set. Set cl-llm-agent-gemini:*gemini-api-key*."))

  (let ((url (format nil "~A?key=~A" *gemini-base-url* api-key))
        (data (list :|contents| (list (list :|parts| (list (list :|text| prompt)))))))
    (handler-case
        (let* ((response-bytes (http-post-json url data))
               (response-str (if (typep response-bytes '(vector (unsigned-byte 8)))
                               (convert-bytes-to-string response-bytes)
                               response-bytes))
               (response-json (parse-json response-str)))
          (if (getf response-json :error)
              (error "Gemini API Error: ~A" (getf response-json :error))
              (let ((candidates (getf response-json :|candidates|)))
                (if candidates
                    (let ((content (getf (first candidates) :|content|)))
                      (if content
                          (let ((parts (getf content :|parts|)))
                            (if parts
                                (getf (first parts) :|text|)
                                (error "Gemini response structure error: No parts in content.")))
                          (error "Gemini response structure error: No content in candidate.")))
                    (error "Gemini response structure error: No candidates in response.")))))
      (error (c)
        (error "Error communicating with Gemini API: ~A" c)))))

;;  (cl-llm-agent-gemini:gemini-generate-content "add 2 + 3")

