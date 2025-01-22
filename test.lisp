(ql:quickload :cl-llm-agent)

(setf cl-llm-agent-gemini::*gemini-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(setf cl-llm-agent-tavily::*tavily-api-key* (uiop:getenv "TAVILY_API_KEY"))

;; Create a context object
(defvar my-context (cl-llm-agent:make-context))

;; Set some initial data in the context
(cl-llm-agent:context-set my-context "current-task" "researching restaurants")
(cl-llm-agent:context-set my-context "user-location" "Paris")

;; Create a Gemini Agent and pass the context
(defvar my-agent (cl-llm-agent:make-agent 'cl-llm-agent::gemini-agent
                                         :context my-context))

;; Set the API keys (assuming there are accessor methods)
;(setf (cl-llm-agent:agent-gemini-api-key my-agent) cl-llm-agent-gemini:*gemini-api-key*)
;(setf (cl-llm-agent:agent-tavily-api-key my-agent) cl-llm-agent-tavily:*tavily-api-key*)


(cl-llm-agent:agent-register-tool my-agent 'cl-llm-agent::tool-read-directory)
(cl-llm-agent:agent-register-tool my-agent 'cl-llm-agent::tool-read-file)

;; Agent interaction - the agent can now access and modify its context
(cl-llm-agent:agent-converse my-agent "Find restaurants based on my current task and location stored in the context.")

;; You can also access the context directly from outside the agent:
(format t "~%Current Task from Context: ~A~%" (cl-llm-agent:context-get my-context "current-task"))

;; Example of setting context from outside:
(cl-llm-agent:context-set my-context "user-cuisine-preference" "Italian")

;; Next conversation turn - the agent can use the updated context
(cl-llm-agent:agent-converse my-agent "Now refine the restaurant search to Italian cuisine.")

;; Example of removing from context
(cl-llm-agent:context-remove my-context "user-location")
(format t "~%User Location from Context after removal: ~A~%" (context-get my-context "user-location")) ; Will be NIL