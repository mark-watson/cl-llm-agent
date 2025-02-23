(asdf:defsystem cl-llm-agent
  :description "Common Lisp library for LLM agent platform"
  :author "Mark Watson"
  :license "Apache 2"
  :version "0.1.0"
  :serial t
  :depends-on (:drakma
               :cl-json
               :babel :gemini :tavily)
  :components ((:file "package")
               ;;(:file "utils")
               ;;(:file "gemini" :depends-on (:utils))
               ;;(:file "tavily" :depends-on (:utils))
               ;;(:file "tools")
               (:file "agent")))

