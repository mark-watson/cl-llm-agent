(ns clj-llm-agent.test-script
  (:require [clj-llm-agent.context :as ctx]
            [clj-llm-agent.agent-generic :as generic]
            [clj-llm-agent.tools :as tools]
            ;; Require agent-gemini to ensure its multimethods are loaded
            [clj-llm-agent.agent-gemini]))


(defn -main [& args]
  (println "--- Clojure LLM Agent Test Script ---")

  ;; 1. Create a context object
  (def my-context (ctx/make-context))
  (println "\n--- Initial Context ---")
  (ctx/display-context my-context "Initial MyContext:")

  ;; 2. Set some initial data in the context
  (ctx/context-set my-context :current-task "researching Clojure libraries")
  (ctx/context-set my-context :user-location "Remote")
  (println "\n--- Context after setting initial data ---")
  (ctx/display-context my-context "MyContext after initial sets:")

  ;; 3. Create a Gemini Agent and pass the context
  (def my-agent (generic/make-agent :gemini-agent
                                    {:name "TestGeminiAgent"
                                     :context my-context}))
  (println "\n--- Agent Created ---")
  (println "Agent Name:" (:name my-agent))
  (println "Agent Type:" (:agent-type my-agent))

  ;; 4. Try extracting context from agent and display
  (let [agent-ctx (:context my-agent)]
    (ctx/display-context agent-ctx "Context fetched from agent:"))
  (ctx/display-context my-context "Original my-context (should be the same object for atom-based context):")

  (println "\n--- Agent Conversation (Verbose Mode) ---")
  (binding [generic/*agent-verbose* true]
    (println "\n--- Conversation 1: Requesting file read (mocked tool call) ---")
    (let [response1 (generic/agent-converse my-agent "Please read the file 'example.txt'.")]
      (println "\nAgent Response 1:" response1)
      (ctx/display-context my-context "Context after conversation 1:"))

    (println "\n--- Conversation 2: Generic query (mocked LLM response) ---")
    (let [response2 (generic/agent-converse my-agent "What are the latest AI advancements?")]
      (println "\nAgent Response 2:" response2)
      (ctx/display-context my-context "Context after conversation 2:")))

  (println "\n--- Agent Conversation (Normal Mode) ---")
  (binding [generic/*agent-verbose* false]
    (println "\n--- Conversation 3: Summarize text (mocked tool call via mocked LLM) ---")
    ;; The Gemini placeholder returns a tool call for tool-read-file.
    ;; To test tool-summarize, we would need to adjust the mock or have a more sophisticated mock LLM.
    ;; For now, this will again trigger the mocked tool-read-file.
    (let [response3 (generic/agent-converse my-agent "Summarize the content of 'test.txt'.")]
      (println "\nAgent Response 3:" response3)
      (ctx/display-context my-context "Context after conversation 3:")))

  ;; 5. Accessing context directly
  (println "\n--- Direct Context Access ---")
  (println "Current Task from Context:" (ctx/context-get my-context :current-task))

  ;; 6. Example of setting context from outside
  (ctx/context-set my-context :user-language-preference "Clojure")
  (println "\n--- Context after setting language preference ---")
  (ctx/display-context my-context "MyContext after setting language preference:")

  (println "\n--- Listing Registered Tools ---")
  (doseq [tool (tools/list-tools)]
    (println tool))

  (println "\n--- End of Test Script ---"))

;; To run this test script (e.g., from a REPL or using clojure -M -m clj-llm-agent.test-script):
;; (clj-llm-agent.test-script/-main)
