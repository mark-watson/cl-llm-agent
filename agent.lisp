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

(defun display-context (context &optional (message "Context contents:"))
  "Pretty prints the contents of a context object."
  (format t "~A~%" message)
  (let ((data (context-data context)))
    (if (hash-table-p data)
        (loop for key being the hash-key of data
		using (hash-value value)
              do (format t "  ~A: ~A~%" key value))
        (format t "  Invalid context object~%"))))

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

(defun make-agent (agent-type &rest initargs &key context)
  "Creates an instance of an agent type."
  (apply #'make-instance agent-type (append (list :context context) initargs)))

(defun make-prompt-string ()
  (with-output-to-string (stream)
    (format stream "tools:~%")
    (dolist (tool (list-tools))
      (format stream "  ~a: ~a~%"
              (getf tool :name)
              (getf tool :description)))))

(defun remove-json-markdown (text)
  "Removes common markdown formatting from a JSON string."
  (let* ((trimmed-text (string-trim '(#\Space #\Newline #\Return #\Tab) text))
         (start-pos (position #\{ trimmed-text))
         (end-pos   (position #\} trimmed-text :from-end t)))
    (if (and start-pos end-pos (< start-pos end-pos))
        (subseq trimmed-text start-pos (1+ end-pos))
        text))) ;; Return original if no JSON found.


(defmethod agent-converse ((agent base-agent) user-input)
  "Handles a conversation turn with the agent."
  (format t "&* * agent-converse: ~A~%" user-input)
  (display-context (cl-llm-agent:agent-context agent) "Context at start of agent-converse call")

  (let* ((tool-descriptions (list-tools))
         (tool-prompt (make-prompt-string))
         (prompt (format nil "~A~%User Input: ~A~%~%Assistant, you can use these tools if needed. If you want to use a tool, respond ONLY in a JSON format like: {\"action\": \"tool_name\", \"parameters\": {\"param1\": \"value1\"}} or for multiple sequential tools: {\"actions\": [{\"action\": \"tool_name1\", \"parameters\": {\"param1\": \"value1\"}}, {\"action\": \"tool_name2\", \"parameters\": {\"param1\": \"PREV_RESULT\"}}]}. Use PREV_RESULT to indicate where the previous tool's output should be used. If you don't need a tool, just respond naturally." tool-prompt user-input))
         (llm-response (agent-llm-call agent prompt))
         (cleaned-response (remove-json-markdown llm-response)))

    (format t "~%LLM Response: ~A~%" llm-response)
    (format t "~%Cleaned LLM Response: ~A~%" cleaned-response)

    (let ((action-request (cl-llm-agent-utils:parse-json cleaned-response)))
      (format t "* agent-converse: action-request = ~A~%" action-request)
      (if (listp action-request)
          (let ((actions (if (assoc :ACTIONS action-request :test #'equal)
                             (cdr (assoc :ACTIONS action-request :test #'equal))
                             (list action-request))))
            (format t "~%debug: actions: ~A~%" actions)
            (let ((prev-result nil))
              (loop for action in actions
                    do (let* ((action-name (cdr (assoc :ACTION action :test #'equal)))
                              (parameters (cdr (assoc :PARAMETERS action :test #'equal)))
                              (param-values (loop for (param-name . param-value) in parameters
						  collect (if (string= param-value "PREV_RESULT")
                                prev-result
                                param-value))))
                         (setf prev-result 
                               (execute-tool action-name param-values))))
              (format nil "Tools executed. Final result: ~A" prev-result)))
          (format nil "Agent response: ~A" cleaned-response)))))

(defun get-tool-function (tool-name)
  "Retrieves the function associated with a given tool name."
  (let ((tool-entry
	  (or
	   (find tool-name (cl-llm-agent::list-tools) :key (lambda (entry) (getf entry :name)) :test #'string=)
	   (find (substitute #\- #\_ tool-name)
		 (cl-llm-agent::list-tools) :key (lambda (entry) (getf entry :name)) :test #'string=))))
    (if tool-entry
        (getf tool-entry :function)
        nil)))

;; Example execute-tool
(defun execute-tool (tool-name parameters)
  "Example of tool execution. Replace with actual tool logic."
  (format t "~%Executing tool ~A with params ~A~%" tool-name parameters)
  (let ((tool-function (get-tool-function tool-name)))
    (format t "  tool-function: ~A~%" tool-function)
    (if tool-function
        (let()
          (format t "~%Found tool function, calling it...parameters = ~A~%" parameters)
          (princ (apply tool-function parameters)))
	(format nil "Unknown tool: ~A" tool-name))))


;; --- Concrete Agent Example using Gemini and tavily ---

(define-agent gemini-agent
    (:bases (base-agent))
  ())

;; Update the agent-llm-call function to use CLOS
(defun agent-llm-call (agent prompt)
  (if (typep agent 'gemini-agent)
      (gemini-generate-content prompt)
      (error "LLM call not implemented for this agent type")))