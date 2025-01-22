(asdf:defsystem cl-llm-agent
  :description "Common Lisp library for LLM agent platform"
  :author "You"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :depends-on (:drakma
               :cl-json)
  :components ((:file "package")
               (:file "utils")
               (:file "gemini" :depends-on (:utils))
               (:file "tavily" :depends-on (:utils))
               (:file "tools" :depends-on (:utils))
               (:file "agent" :depends-on (:gemini :tavily :tools :utils))))

