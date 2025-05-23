(ns clj-llm-agent.tests
  (:require [clojure.test :refer :all]
            [clj-llm-agent.context :as ctx]
            [clj-llm-agent.tools :as tools]))

(deftest context-set-get-remove-test
  (let [test-ctx (ctx/make-context)]
    (testing "Setting and getting a value"
      (ctx/context-set test-ctx :foo 42)
      (is (= (ctx/context-get test-ctx :foo) 42) "Value should be retrieved correctly."))
    (testing "Removing a value"
      (ctx/context-remove test-ctx :foo)
      (is (nil? (ctx/context-get test-ctx :foo)) "Value should be nil after removal."))))

(deftest tool-registration-and-execution-test
  (let [dummy-tool-name "dummy-multiply-tool"
        original-registry-value (atom nil)] ; To store any pre-existing tool with the same name

    (testing "Registering and executing a new tool"
      ;; Store and remove if a tool with this name already exists, for test idempotency
      (when (get @tools/tool-registry dummy-tool-name)
        (reset! original-registry-value (get @tools/tool-registry dummy-tool-name))
        (swap! tools/tool-registry dissoc dummy-tool-name))

      (tools/register-tool dummy-tool-name
                           :description "A dummy tool that multiplies its input by 2."
                           :parameters [:x]
                           :parameter-example "x: number"
                           :function (fn [x] (* 2 x)))
      (is (= (tools/execute-tool dummy-tool-name 21) 42) "Tool should execute and return correct result."))

    (testing "Cleaning up the dummy tool"
      (swap! tools/tool-registry dissoc dummy-tool-name)
      (is (nil? (get @tools/tool-registry dummy-tool-name)) "Dummy tool should be removed from registry.")
      
      ;; Restore original tool if one was temporarily removed
      (when @original-registry-value
        (tools/register-tool dummy-tool-name
                             :description (:description @original-registry-value)
                             :parameters (:parameters @original-registry-value)
                             :parameter-example (:parameter-example @original-registry-value)
                             :function (:function @original-registry-value))))
    
    (testing "Executing a non-existent tool"
        (is (thrown? clojure.lang.ExceptionInfo (tools/execute-tool "non-existent-tool" 123))
            "Executing a non-existent tool should throw an ExceptionInfo."))))

;; To run these tests, you would typically use a test runner.
;; For example, in a REPL:
;; (require 'clj-llm-agent.tests)
;; (clojure.test/run-tests 'clj-llm-agent.tests)
