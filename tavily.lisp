(in-package :cl-llm-agent-tavily) ;; Note: package name was initially tavily, correcting to tavily based on request

(defvar *tavily-api-key* (uiop:getenv "TAVILY_API_KEY")
  "Your Tavily Search API key. Set this before using Tavily functions.  Alternatively set TAVILY_API_KEY environment variable.")

(defvar *tavily-api-url* "https://api.tavily.com/search"
  "Base URL for Tavily Search API.")

(defun make-tavily-json-payload (query)
  "Helper function to create JSON payload for Tavily API request, mirroring Racket's json-helper."
  (cl-json:encode-json-to-string
   (list :|api_key| (or *tavily-api-key* (uiop:getenv "TAVILY_API_KEY")) ; Prioritize variable, then env var
         :|query| query
         :|max_results| 5)))

(defun filter-tavily-response-item (response-item)
  "Helper function to filter Tavily API response item, mirroring Racket's filter-response."
  (list (gethash "url" response-item)
        (gethash "title" response-item)
        (gethash "content" response-item)))

(defun tavily-search (query &key (api-key *tavily-api-key*))
  "Performs a search using the Tavily Search API, rewritten from Racket."
  (let* ((api-key-to-use (or api-key (uiop:getenv "TAVILY_API_KEY"))) ; Get API key, prioritize arg then env
         (api-url *tavily-api-url*)
         (prompt-data (make-tavily-json-payload query)))

    (unless api-key-to-use
      (error "Tavily API key is not set. Set cl-llm-agent-tavily:*tavily-api-key* or TAVILY_API_KEY environment variable."))

    (handler-case
        (let* ((response-str (http-post-json api-url prompt-data
                                              `((:content-type "application/json")))) ; Set content-type for JSON
               (response-json (parse-json response-str)))
          (if (getf response-json :error) ; Check for error using getf (like original)
              (error "Tavily API Error: ~A" (getf response-json :error))
              ;; Process and return search results, using hash-table access and mapcar
              (mapcar #'filter-tavily-response-item (gethash "results" response-json))))
      (error (e)
        (error "Error communicating with Tavily API: ~A" e)))))


;; Example usage (for testing - remove or comment out in library)
;; (comment
;;   (setf cl-llm-agent-tavily:*tavily-api-key* "YOUR_TAVILY_API_KEY") ; Or set env var TAVILY_API_KEY
;;   (pprint (tavily-search "Fun things to do in Sedona Arizona")))
;; )