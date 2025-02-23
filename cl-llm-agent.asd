(asdf:defsystem cl-llm-agent
  :description "Common Lisp library for LLM agent platform"
  :author "Mark Watson"
  :license "Apache 2"
  :version "0.1.0"
  :serial t
  :depends-on (:cl-json
               :gemini
               :tavily)
  :components ((:file "package")
               (:file "agent")))

