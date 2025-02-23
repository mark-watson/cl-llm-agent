(defpackage :cl-llm-agent
  (:use :cl :tavily :gemini)
  (:export #:define-agent
           #:make-agent
           #:agent-converse
           #:register-tool
           #:agent-tools
           #:agent-llm-call
           #:agent-search
           #:make-context
           #:context-data
           #:context-remove
           #:display-context
           #:agent-context))
