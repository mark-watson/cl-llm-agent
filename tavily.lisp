(in-package :cl-llm-agent-tavily)

(defvar *tavily-api-key* (uiop:getenv "TAVILY_API_KEY")
  "Your Tavily Search API key. Set this before using Tavily functions.  Alternatively set TAVILY_API_KEY environment variable.")

(defvar *tavily-api-url* "https://api.tavily.com/search"
  "Base URL for Tavily Search API.")

;; Define the conversion function using the imported babel function directly
(defun convert-bytes-to-string (bytes)
  (octets-to-string bytes :encoding :utf-8))

(defun make-tavily-json-payload (query)
  "Helper function to create JSON payload for Tavily API request."
  (jonathan:to-json 
   (list :|api_key| (or *tavily-api-key* (uiop:getenv "TAVILY_API_KEY"))
         :|query| query
         :|max_results| 5)))

(defun filter-tavily-response-item (result)
  "Helper function to filter Tavily API response item, mirroring Racket's filter-response."
  ;;(format t "** result: ~A~%" result)
  (list (cdr (nth 0 result))
        (cdr (nth 1 result))
        (cdr (nth 2 result))))

(defun tavily-search (query &key (api-key *tavily-api-key*))
  "Performs a search using the Tavily Search API, rewritten from Racket."
  (format t "~%* Calling tavily-search with qery: ~A~%" query)
  (let* ((api-key-to-use (or api-key (uiop:getenv "TAVILY_API_KEY"))) ; Get API key, prioritize arg then env
         (api-url *tavily-api-url*)
         (prompt-data (make-tavily-json-payload query)))

    (unless api-key-to-use
      (error "Tavily API key is not set. Set cl-llm-agent-tavily:*tavily-api-key* or TAVILY_API_KEY environment variable."))

    (handler-case
        (let ((response-str (dex:post api-url
                                      :headers '(("Content-Type" . "application/json"))
                                      :content prompt-data)))
           (let ((response-json (parse-json response-str)))
             (if (getf response-json :error) ; Check for error using getf (like original)
                (error "Tavily API Error: ~A" (getf response-json :error))
              ;; Process and return search results, using hash-table access and mapcar
              (let ((uri-title-content-list
                     (mapcar #'filter-tavily-response-item (cdr (assoc :RESULTS response-json)))))
                ;; let's just return concatenated content
                (format nil "~{~a~%~}" (mapcar #'caddr uri-title-content-list))))))
      (error (c)
        (error "Error communicating with Tavily API: ~A" c)))))

;; (cl-llm-agent-tavily:tavily-search "Fun things to do in Sedona Arizona")
