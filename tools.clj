(ns clj-llm-agent.tools
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defonce tool-registry (atom {})) ; Use defonce to avoid re-initializing if the ns is reloaded

(defn register-tool
  "Register a tool NAME with metadata and FUNCTION."
  [name & {:keys [description parameters parameter-example function] :as tool-data}]
  (swap! tool-registry assoc name (assoc tool-data :name name)))

(defn list-tools
  "Return a list of registered tools with metadata."
  []
  (vals @tool-registry))

(defn execute-tool
  "Execute tool NAME with ARGS, checking parameter counts."
  [name & args]
  (let [tool-data (get @tool-registry name)]
    (if-not tool-data
      (throw (ex-info (str "Tool " name " not found") {:tool-name name}))
      (let [func (:function tool-data)
            expected-params (:parameters tool-data)
            actual-args args]
        (if-not (= (count expected-params) (count actual-args))
          (throw (ex-info (str "Tool " name " expected " (count expected-params) " args but got " (count actual-args))
                          {:tool-name name
                           :expected (count expected-params)
                           :actual (count actual-args)}))
          (apply func actual-args))))))

(defmacro define-tool
  "Convenience macro to register a tool.
  Example: (define-tool "my-tool"
             "Does something cool."
             [:arg1 :arg2]
             "arg1: string, arg2: number"
             (fn [arg1 arg2] (str arg1 "-" arg2)))"
  [name description parameters parameter-example function-body]
  `(register-tool ~name
                  :description ~description
                  :parameters ~parameters
                  :parameter-example ~parameter-example
                  :function ~function-body))

;; Placeholder for helper-summarize - requires Gemini client
(defn helper-summarize [text]
  (let [prompt (str "Summarize the following text:\n" text)]
    (println "--- Summarize Tool (Placeholder) ---")
    (println prompt)
    (println "--- End Summarize Tool ---")
    (str "Summary of: '" text "' (placeholder - requires Gemini integration)")))

(register-tool "tool-summarize"
               :description "Summarize text using Gemini (placeholder)."
               :parameters [:text]
               :parameter-example "text: string"
               :function helper-summarize)

;; Placeholder for tool-search-web - requires Tavily client
(defn helper-search-web [query]
  (println "--- Search Web Tool (Placeholder) ---")
  (println (str "Query: " query))
  (println "--- End Search Web Tool ---")
  (str "Search results for: '" query "' (placeholder - requires Tavily integration)"))

(register-tool "tool-search-web"
               :description "Search the web with Tavily (placeholder)."
               :parameters [:query]
               :parameter-example "query: string"
               :function helper-search-web)

;; Predefined helper functions and their registrations
(defn helper-read-directory
  "List files in DIR excluding hidden or backup files."
  [directory-path-str]
  (let [dir (io/file directory-path-str)]
    (if (.exists dir)
      (if (.isDirectory dir)
        (->> (.listFiles dir)
             (map #(.getName %))
             (remove (fn [n]
                       (or (.startsWith n ".") ; hidden files
                           (.startsWith n "#") ; Emacs backup files
                           (.endsWith n "~")))) ; other backup files
             vec)
        (throw (ex-info (str "Not a directory: " directory-path-str) {:path directory-path-str})))
      (throw (ex-info (str "Directory not found: " directory-path-str) {:path directory-path-str})))))

(register-tool "tool-read-directory"
               :description "Reads the contents of a directory."
               :parameters [:directory-path]
               :parameter-example "directory-path: string"
               :function helper-read-directory)

(defn helper-read-file
  "Return the contents of FILE as a string, or error if missing."
  [file-path-str]
  (let [file (io/file file-path-str)]
    (if (.exists file)
      (if (.isFile file)
        (slurp file)
        (throw (ex-info (str "Not a regular file: " file-path-str) {:path file-path-str})))
      (throw (ex-info (str "File not found: " file-path-str) {:path file-path-str})))))

(register-tool "tool-read-file"
               :description "Reads the contents of a file."
               :parameters [:file-path]
               :parameter-example "file-path: string"
               :function helper-read-file)
