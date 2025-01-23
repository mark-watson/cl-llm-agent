(in-package :cl-llm-agent)

(defstruct context
  (data (make-hash-table :test #'equal)))

(defun create-context ()
  (make-context))

(defun context-set (context key value)
  (setf (gethash key (context-data context)) value))

(defun context-get (context key)
  (gethash key (context-data context)))

(defun context-remove (context key)
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

(defmethod agent-converse ((agent base-agent) user-input)
  "Handles a conversation turn with the agent."
  (let* ((tool-descriptions (list-tools)) ; Get descriptions of registered tools
         (tool-prompt (format nil "Available tools:~%~{~A: ~A~%~}" (loop for tool in tool-descriptions
                                                                     collect (list (getf tool :name) (getf tool :description)))))
         (prompt (format nil "~A~%User Input: ~A~%~%Assistant, you can use these tools if needed. If you want to use a tool, respond in a JSON format like: {\"action\": \"tool_name\", \"parameters\": {\"param1\": \"value1\", \"param2\": \"value2\"}}. If you don't need a tool, just respond naturally." tool-prompt user-input))
         (llm-response (agent-llm-call agent prompt)))

    (format t "~%LLM Response: ~A~%" llm-response)

    (handler-case
        (let ((action-request (cl-llm-agent-utils:parse-json llm-response))) ; Try to parse as JSON tool request
          (if (and (hash-table-p action-request) (gethash "action" action-request))
              (let ((action-name (gethash "action" action-request))
                    (parameters (gethash "parameters" action-request)))
                (format t "~%Tool Requested: ~A with parameters: ~A~%" action-name parameters)
                (let ((tool-result (execute-tool action-name (loop for param-name being the hash-key of parameters
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


(defgeneric agent-llm-call (agent prompt)
  (:documentation "Abstract method for making LLM calls. Needs to be specialized for specific LLMs."))

(defmethod agent-llm-call ((agent base-agent) prompt)
  (error "agent-llm-call not implemented for base-agent. Subclass base-agent and implement this method."))


(defgeneric agent-search (agent query)
  (:documentation "Abstract method for performing search. Needs to be specialized for specific search APIs."))

(defmethod agent-search ((agent base-agent) query)
  (error "agent-search not implemented for base-agent. Subclass base-agent and implement this method."))


;; --- Concrete Agent Example using Gemini and tavily ---

(define-agent gemini-agent
  (:bases (base-agent))
  ((gemini-api-key :initarg :gemini-api-key :accessor gemini-api-key)
   (tavily-api-key :initarg :tavily-api-key :accessor tavily-api-key)))


(defmethod initialize-instance :after ((agent gemini-agent) &key gemini-api-key tavily-api-key &allow-other-keys)
  (setf cl-llm-agent-gemini:*gemini-api-key* gemini-api-key)
  (setf cl-llm-agent-tavily:*tavily-api-key* tavily-api-key))


(defmethod agent-llm-call ((agent gemini-agent) prompt)
  (gemini-generate-content prompt :api-key (gemini-api-key agent)))

(defmethod agent-search ((agent gemini-agent) query)
  (tavily-search query :api-key (tavily-api-key agent)))

;; Remove these structure definitions
;; (defstruct base-agent
;;   (tools (make-hash-table :test #'equal))
;;  name
;;  context)

;; (defstruct (gemini-agent (:include base-agent))
;;   gemini-api-key
;;   tavily-api-key)

;; Remove this entire function as it's using the old struct-based approach
(defun make-agent (agent-type &key name context gemini-api-key tavily-api-key)
  (case agent-type
    (gemini-agent 
     (let ((agent (make-gemini-agent 
                  :name name 
                  :context (or context (make-context))
                  :gemini-api-key gemini-api-key
                  :tavily-api-key tavily-api-key)))
       (setf cl-llm-agent-gemini:*gemini-api-key* gemini-api-key
             cl-llm-agent-tavily:*tavily-api-key* tavily-api-key)
       agent))
    (otherwise (error "Unknown agent type: ~A" agent-type))))

(defun agent-register-tool (agent tool-name)
  (let ((tool-data (gethash tool-name cl-llm-agent-tools:*tool-registry*)))
    (if tool-data
        (setf (gethash tool-name (agent-tools agent)) tool-data)
        (error "Tool ~A not found in global tool registry." tool-name))))

(defun agent-converse (agent user-input)
  (let* ((tool-descriptions (list-tools))
         (tool-prompt (format nil "Available tools:~%~{~A: ~A~%~}" 
                            (loop for tool in tool-descriptions
                                  collect (list (getf tool :name) 
                                              (getf tool :description)))))
         (prompt (format nil "~A~%User Input: ~A~%~%Assistant, you can use these tools if needed. If you want to use a tool, respond in a JSON format like: {\"action\": \"tool_name\", \"parameters\": {\"param1\": \"value1\", \"param2\": \"value2\"}}. If you don't need a tool, just respond naturally." 
                        tool-prompt user-input))
         (llm-response (agent-llm-call agent prompt)))
    
    (format t "~%LLM Response: ~A~%" llm-response)
    (handler-case
        (let ((action-request (cl-llm-agent-utils:parse-json llm-response)))
          (if (and (hash-table-p action-request) 
                   (gethash "action" action-request))
              (let ((action-name (gethash "action" action-request))
                    (parameters (gethash "parameters" action-request)))
                (format t "~%Tool Requested: ~A with parameters: ~A~%" 
                        action-name parameters)
                (let ((tool-result (execute-tool action-name 
                                               (loop for param-name being the hash-key of parameters
                                                     using (hash-value param-value)
                                                     collect param-value))))
                  (format t "~%Tool Result: ~A~%" tool-result)
                  (format nil "Tool '~A' executed. Result: ~A" 
                          action-name tool-result)))
              llm-response))
        (error (e)
          llm-response))))

;; Update the agent-llm-call function to use CLOS
(defun agent-llm-call (agent prompt)
  (if (typep agent 'gemini-agent)
      (gemini-generate-content prompt 
                              :api-key (gemini-api-key agent))
      (error "LLM call not implemented for this agent type")))

;; Update the agent-search function to use CLOS
(defun agent-search (agent query)
  (if (typep agent 'gemini-agent)
      (tavily-search query 
                     :api-key (tavily-api-key agent))
      (error "Search not implemented for this agent type")))
