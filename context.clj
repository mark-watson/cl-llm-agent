(ns clj-llm-agent.context
  (:require [clojure.pprint :as pp]))

(defn make-context
  "Creates a new agent context (an atom containing an empty map)."
  []
  (atom {}))

(defn context-set
  "Sets key to value in the context (atom holding a map)."
  [ctx key value]
  (swap! ctx assoc key value))

(defn context-get
  "Retrieves the value for key from the context (atom holding a map)."
  [ctx key]
  (get @ctx key))

(defn context-remove
  "Removes key from the context (atom holding a map)."
  [ctx key]
  (swap! ctx dissoc key))

(defn display-context
  "Pretty prints the contents of the context."
  [ctx &optional message]
  (when message
    (println message))
  (pp/pprint @ctx))
