(in-package :cl-llm-agent)

(defclass context ()
  ((data :initform (make-hash-table :test #'equal)
         :accessor context-data))
  (:documentation "Context class for storing key-value pairs"))

(defun make-context ()
  (make-instance 'context))

(defgeneric context-set (context key value)
  (:documentation "Set a value in the context"))

(defmethod context-set ((context context) key value)
  (setf (gethash key (context-data context)) value))

(defgeneric context-get (context key)
  (:documentation "Get a value from the context"))

(defmethod context-get ((context context) key)
  (gethash key (context-data context)))

(defgeneric context-remove (context key)
  (:documentation "Remove a key from the context"))

(defmethod context-remove ((context context) key)
  (remhash key (context-data context)))


(defvar *agent-registry* (make-hash-table :test #'equal)
  "Registry to store defined agent types.")

(defmacro define-agent (agent-name &body body)
  "Defines a new agent type."
  (let ((bases nil)
        (rest-body body))
    (when (and body (eq (first (first body)) :bases)) ; Check if first form is (:bases ...)
      (setf bases (second (first body)) ; Extract superclass list
            rest-body (rest body)))     ; Remove :bases form from body

    `(defclass ,agent-name ,bases  ; Place superclasses correctly
      ,@rest-body)))           ; Place the rest of the body (options and slots)


(defclass base-agent ()
  ((tools :initform (make-hash-table :test #'equal) :accessor agent-tools)
   (name :initarg :name :accessor agent-name)
   (context :initarg :context :accessor agent-context)) ; Add context slot
  (:documentation "Base class for agents."))

(defmethod initialize-instance :after ((agent base-agent) &key context &allow-other-keys)
  ;; If context is not provided, create a default one
  (unless (slot-boundp agent 'context)
    (setf (slot-value agent 'context) (make-context))))

;; Remove or comment out the old make-agent function that uses case
(defun make-agent (agent-type &rest initargs &key context)
  "Creates an instance of an agent type."
  (apply #'make-instance agent-type (append (list :context context) initargs)))

(defmethod agent-register-tool ((agent base-agent) tool-name)
  "Registers a tool with the agent."
  (let ((tool-data (gethash tool-name cl-llm-agent-tools:*tool-registry*)))
    (if tool-data
        (setf (gethash tool-name (agent-tools agent)) tool-data)
        (error "Tool ~A not found in global tool registry." tool-name))))

(defun make-prompt-string ()
  (with-output-to-string (stream)
    (format stream "tools:~%")
    (dolist (tool (list-tools))
      (format stream "  ~a: ~a~%"
              (getf tool :name)
              (getf tool :description)))))

(defvar *tool-prompt* "Available tools:
  TOOL-SEARCH-WEB: Search the web.
  TOOL-READ-DIRECTORY: Reads the contents of a directory.
  TOOL-READ-FILE: Reads the contents of a file.
")

(defmethod agent-converse ((agent base-agent) user-input)
  "Handles a conversation turn with the agent."
  (format t "&* * agent-converse: ~A~%" user-input)
  (let* ((tool-descriptions (list-tools)) ; Get descriptions of registered tools
         (tool-prompt (make-prompt-string)) ;; *tool-prompt*)
         (prompt (format nil "~A~%User Input: ~A~%~%Assistant, you can use these tools if needed. If you want to use a tool, respond in a JSON format like: {\"action\": \"tool_name\", \"parameters\": {\"param1\": \"value1\", \"param2\": \"value2\"}}. If you don't need a tool, just respond naturally. Do not include markdown formatting for JSON output!" tool-prompt user-input))
         (llm-response (agent-llm-call agent prompt)))

    (format t "~%LLM Response: ~A~%" llm-response)

    (handler-case
        (let ((action-request (cl-llm-agent-utils:parse-json llm-response))) ; Try to parse as JSON tool request
          (if (and (hash-table-p action-request) (gethash "action" action-request))
              (let ((action-name (gethash "action" action-request))
                    (parameters (gethash "parameters" action-request)))
                (format t "~%Tool Requested: ~A with parameters: ~A~%" action-name parameters)
                (let ((tool-result
                        (execute-tool action-name
                          (loop for param-name being the hash-key of parameters
                                                                  using (hash-value param-value)
                                                                  collect param-value)))) ; Order of parameters might be important - improve this
                  (format t "~%Tool Result: ~A~%" tool-result)
                  ;; Optionally feed tool result back to LLM for next turn
                  (format nil "Tool '~A' executed. Result: ~A" action-name tool-result) ; For simple return, can be improved with feedback loop
                  ))
              ;; No tool requested, return LLM response directly
              llm-response))
        (error (e)
          ;; Not a JSON tool request, treat as natural language response
          llm-response))))


;; --- Concrete Agent Example using Gemini and tavily ---

(define-agent gemini-agent
  (:bases (base-agent))
  ())

;; Update the agent-llm-call function to use CLOS
(defun agent-llm-call (agent prompt)
  (if (typep agent 'gemini-agent)
      (gemini-generate-content prompt)
      (error "LLM call not implemented for this agent type")))

;; Update the agent-search function to use CLOS
(defun agent-search (agent query)
  (if (typep agent 'gemini-agent)
      (tavily-search query)
      (error "Search not implemented for this agent type")))
