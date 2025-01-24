(in-package :cl-llm-agent)

(defun pp-hash (message h)
  "Prints a message followed by the contents of a hash table."
  (format t "~A~%" message)
  (when (hash-table-p h)
    (loop for key being the hash-key of h
          using (hash-value value)
          do (format t "  ~A: ~A~%" key value)))
  (unless (hash-table-p h)
    (format t "  Not a hash table: ~A~%" h)))

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

(defvar *XXXXtool-prompt* "Available tools:
  TOOL-SEARCH-WEB: Search the web.
  TOOL-READ-DIRECTORY: Reads the contents of a directory.
  TOOL-READ-FILE: Reads the contents of a file.
")


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
  (let* ((tool-descriptions (list-tools))
         (tool-prompt (make-prompt-string))
         (prompt (format nil "~A~%User Input: ~A~%~%Assistant, you can use these tools if needed. If you want to use a tool, respond ONLY in a JSON format like: {\"action\": \"tool_name\", \"parameters\": {\"param1\": \"value1\", \"param2\": \"value2\"}}. If you don't need a tool, just respond naturally. Do not include markdown formatting for JSON output! An example of JSON output: {\"action\": \"search_restaurant\", \"parameters\": {\"cuisine\": \"Italian\", \"location\": \"London\"}}.  If you require more information, then respond naturally" tool-prompt user-input))
         (llm-response (agent-llm-call agent prompt))
         (cleaned-response (remove-json-markdown llm-response)))

    (format t "~%LLM Response: ~A~%" llm-response)
    (format t "~%Cleaned LLM Response: ~A~%" cleaned-response)

    (handler-case
        (let ((action-request (cl-llm-agent-utils:parse-json cleaned-response)))
          (format t "* agent-converse: action-request = ~A~%" action-request)
         (if (listp action-request)
              (let ((action-name (cdr (assoc :ACTION action-request :test #'equal)))
                    (parameters (cdr (assoc :PARAMETERS action-request :test #'equal))))
                (if (listp parameters)
                    (let ((h (make-hash-table :test #'equal)))
                      (dolist (p parameters)
                        (setf (gethash (symbol-name (car p)) h) (cdr p)))
                      (pp-hash "* function call params" h)
                      (format t "~%Tool Requested: ~A with parameters hashtable: ~S~%" action-name h)
                      (let ((tool-result
                             (execute-tool action-name
                                           (if (listp parameters)
                                               (loop for (param-name . param-value) in parameters
                                                     collect param-value)
                                             (list parameters))))) ; If parameters are a list of pairs, get the values, otherwise, treat parameters as a single value
                        (format t "~%Tool Result: ~A~%" tool-result)
                        (format nil "Tool '~A' executed. Result: ~A" action-name tool-result)
                        ))

                  ;; No tool requested, return LLM response directly
                  (format nil "Agent response: ~A" cleaned-response)  ;; Return original response
                  ))))

        (error (e)
          ;; Not a JSON tool request, treat as natural language response
           (format t "ERROR from agent-converse: ~A~%" e)
            (format nil "Agent response: ~A" cleaned-response)
          ))))


(defun safe-funcall (func-symbol &rest args)
    (let ((func (fdefinition func-symbol)))
        (if func
            (apply func args)
            (format t "Error: ~A does not name a function.~%" func-symbol))))

;; Example execute-tool
(defun execute-tool (tool-name parameters)
  "Example of tool execution. Replace with actual tool logic."
  (format t "~%Executing tool ~A with params ~A~%" tool-name parameters)
  (cond
    ((equal tool-name 'TOOL-SEARCH-WEB)
        (format t "~%Performing Web search for ~A~%" (first parameters))
        (format nil "Search results for: ~A" (first parameters)) )
    (t
      (format nil "Unknown tool: ~A" tool-name)
    )))


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
