;;; cl-llm-agent.asd -- ASDF system definition for cl-llm-agent
(in-package #:asdf-user)

(defsystem "cl-llm-agent"
  :name "cl-llm-agent"
  :version "0.1.0"
  :author "Your Name <you@example.com>"
  :license "MIT"
  :description "A generic LLM-based agent framework for Common Lisp."
  :depends-on ("cl-json" "gemini" "tavily" "uiop" "fiveam")
  :components ((:file "package")
               (:file "context")
               (:file "agent-generic")
               (:file "agent-gemini")
               (:file "tools")))