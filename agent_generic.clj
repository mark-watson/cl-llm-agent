(ns clj-llm-agent.agent-generic
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clj-llm-agent.context :as ctx]
            [clj-llm-agent.tools :as tools]))

;; JSON parsing utilities
(defn parse-json [json-string]
  (try
    (json/read-str json-string :key-fn keyword)
    (catch Exception _ nil))) ; Return nil on failure, like the Lisp version

(defn safe-parse-json [json-string]
  (let [parsed (parse-json json-string)]
    (if parsed
      parsed
      (throw (ex-info (str "Failed to parse JSON: " json-string) {:json-string json-string})))))

(defn strip-markdown-json [text]
  (let [trimmed (str/trim text)
        json-block-regex #"```json\s*([\s\S]+?)\s*```"]
    (if-let [match (re-find json-block-regex trimmed)]
      (second match)
      trimmed)))

;; Base agent record
;; Clojure's defrecord creates a type with named fields, suitable for representing agents.
;; It also generates a constructor function (->BaseAgent) and field accessors.
(defrecord BaseAgent [name context tools agent-type]) ; Added agent-type for multimethod dispatch

;; LLM back-end multimethod
;; Dispatches on the :agent-type field of the agent record.
(defmulti agent-llm-call :agent-type)

;; Default implementation for agent-llm-call (can be overridden by specific agent types)
(defmethod agent-llm-call :default [agent prompt]
  (throw (ex-info (str "LLM call not implemented for agent type: " (:agent-type agent))
                  {:agent agent :prompt prompt})))

;; Agent factory function
(defn make-agent
  "Constructs an agent of a given type with initial properties.
  Example: (make-agent :gemini-agent {:name "MyGemini" :context my-ctx})"
  [agent-type initargs]
  (let [context (or (:context initargs) (ctx/make-context))
        tools-map (or (:tools initargs) {})] ; Tools specific to this agent instance (if any)
    (->BaseAgent (:name initargs) context tools-map agent-type)))


;; Dynamic var for verbose output
(def ^:dynamic *agent-verbose* false)

;; Agent conversation logic
(defn agent-converse [agent user-input]
  (when *agent-verbose*
    (println "[agent-converse] input:" user-input)
    (ctx/display-context (:context agent) "Context start:"))

  (let [tools-info (with-out-str
                     (println "tools:")
                     (doseq [t (tools/list-tools)]
                       (println (str "  " (:name t) ": " (:description t)))))
        prompt (str tools-info "\n"
                    "User Input: " user-input "\n\n"
                    "If using tools, respond in JSON.")
        ;; Placeholder for raw LLM call - this will be specialized by agent type
        raw-llm-response (agent-llm-call agent prompt)
        
        ;; Ensure raw-llm-response is a string before stripping
        clean-json-text (if (string? raw-llm-response)
                           (strip-markdown-json raw-llm-response)
                           (throw (ex-info "LLM response was not a string." {:response raw-llm-response})))
        parsed-response (safe-parse-json clean-json-text)
        
        ;; The Lisp code expects actions to be a list (vector in Clojure),
        ;; even if there's a single action map.
        actions (let [actions-field (:actions parsed-response)]
                  (cond
                    (vector? actions-field) actions-field
                    (map? actions-field) [actions-field] ; Wrap single map in a vector
                    (map? parsed-response) [parsed-response] ; If top-level is the action
                    :else []))]

    (when *agent-verbose*
      (println "[agent-converse] raw-llm-response:" raw-llm-response)
      (println "[agent-converse] clean-json-text:" clean-json-text)
      (println "[agent-converse] parsed-response:" parsed-response)
      (println "[agent-converse] actions:" actions))

    (loop [remaining-actions actions
           prev-result nil
           final-results []]
      (if-let [action (first remaining-actions)]
        (let [action-name (keyword (:action action)) ; Ensure action name is a keyword if tools are registered with keywords
              action-params (:parameters action)
              processed-params (map (fn [p] (if (= p "PREV_RESULT") prev-result p))
                                    (if (sequential? action-params) action-params []))]
          (when *agent-verbose*
            (println "[agent-converse] Executing action:" action-name "with params:" processed-params))
          
          (try
            (let [current-result (apply tools/execute-tool (name action-name) processed-params)]
              (recur (rest remaining-actions) current-result (conj final-results current-result)))
            (catch Exception e
              (when *agent-verbose*
                (println "[agent-converse] Error executing action:" action-name e))
              (recur (rest remaining-actions) (str "Error executing " action-name ": " (.getMessage e)) (conj final-results (str "Error executing " action-name))))))
        ;; When no more actions, decide what to return
        (if (seq final-results)
            (str/join "\n" final-results) ; Return all results or just the last one? Lisp returns `prev`
            (if (string? raw-llm-response) raw-llm-response (str raw-llm-response)))))))


;; The define-agent macro from Lisp is more complex due to CLOS.
;; For Clojure, records and multimethods provide much of the same functionality.
;; A simple version might just be a helper for creating agent factory functions
;; or ensuring consistent record creation, but `make-agent` already handles this.
;; If specific agent types need more complex setup, they can have their own
;; constructor functions.
;; For now, we'll omit a direct Clojure `define-agent` macro, as `make-agent`
;; combined with `defrecord` and `defmulti` covers the core needs.
