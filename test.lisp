;;(ql:quickload :cl-llm-agent)

;; Create a context object
(defvar my-context (cl-llm-agent:make-context))

;; Set some initial data in the context
(setf (gethash "current-task" (cl-llm-agent:context-data my-context))  "researching restaurants")
(setf (gethash "user-location" (cl-llm-agent:context-data my-context)) "Paris")

;; Create a Gemini Agent and pass the context
(defvar my-agent (cl-llm-agent:make-agent 'cl-llm-agent::gemini-agent
                                          :context my-context))

;(cl-llm-agent:agent-converse my-agent "Search the web to find information on AI advancements.")

;; Agent interaction - the agent can now access and modify its context
;(cl-llm-agent:agent-converse my-agent "Find restaurants based on my current task and location stored in the context.")

;; You can also access the context directly from outside the agent:
(format t "~%Current Task from Context: ~A~%" (gethash "current-task" (cl-llm-agent:context-data my-context)))

;; Example of setting context from outside:
(setf (gethash "user-cuisine-preference" (cl-llm-agent:context-data my-context)) "Italian")

;; Next conversation turn - the agent can use the updated context
;(cl-llm-agent:agent-converse my-agent "Now refine the restaurant search to Italian cuisine.")

(cl-llm-agent:agent-converse my-agent "What Lisp source files are in the current directory?")
 