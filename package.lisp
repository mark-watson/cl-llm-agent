(defpackage :cl-llm-agent-utils
  (:use :cl)
  (:export :http-post-json
           :parse-json
           :dexador))

(defpackage :cl-llm-agent-gemini
  (:use :cl :cl-llm-agent-utils)
  (:export :gemini-generate-content
           :*gemini-api-key*
           :*gemini-base-url*))

(defpackage :cl-llm-agent-tavily
  (:use :cl :cl-llm-agent-utils)
  (:import-from :babel :octets-to-string)
  (:export :tavily-search
           :*tavily-api-key*
           :*tavily-api-url*))

(defpackage :cl-llm-agent-tools
  (:use :cl)
  (:export :define-tool
           :execute-tool
           :list-tools
           :*tool-registry*
           :tool-read-directory
           :tool-read-file
           :tool-search-web))

(defpackage :cl-llm-agent
  (:use :cl :cl-llm-agent-gemini :cl-llm-agent-tavily :cl-llm-agent-tools)
  (:export :define-agent
           :make-agent
           :agent-converse
           :register-tool
           :agent-register-tool
           :agent-tools
           :agent-llm-call
           :agent-search
           :*agent-registry*
           :context             ; Export context class
           :make-context        ; Export make-context function
           ;;:context-set         ; Export context-set generic function
           :context-data         ; Export context-get generic function
           :context-remove))    ; Export context-remove generic function

