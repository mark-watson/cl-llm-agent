# cl-llm-agent

cl-llm-agent is a Common Lisp framework for building LLM-based agents that can use tools,
maintain context, and interface with different LLM back-ends.

## Features
- Context management (get, set, remove)
- Tool registration and execution
- Generic agent abstraction with pluggable back-ends
- Example Gemini-based agent
- Testing suite with FiveAM

## Installation
1. Clone the repository.
2. In your Lisp REPL:
   ```lisp
   (ql:quickload :cl-llm-agent)
   ```

## Usage
Define tools and agents, then interact:
```lisp
  (defparameter *ctx* (cl-llm-agent:make-context))
  (cl-llm-agent:context-set *ctx* "task" "summarize text")
  (defparameter *agent*
    (cl-llm-agent:make-agent 'cl-llm-agent::gemini-agent :context *ctx*))
  (cl-llm-agent:agent-converse *agent* "Summarize the project README.")
```

## Testing
Run in your Lisp REPL:
```lisp
  (ql:quickload :cl-llm-agent)
  (asdf:test-system :cl-llm-agent)
```
## Clojure Version

This repository also contains a Clojure port of the Common Lisp LLM agent framework.
The Clojure source files (`.clj`) are located in the root directory and mirror the structure of the Lisp files.

### Dependencies

The Clojure version uses `deps.edn` for dependency management. Key dependencies include:
- `org.clojure/clojure`
- `org.clojure/data.json`

### Running the Clojure Code

#### Unit Tests

To run the automated unit tests (defined in `tests/tests.clj`):
```bash
clojure -M:test
```

#### Manual Test Script

A manual test script is available at `test.clj`. You can run its `-main` function using the Clojure CLI:
```bash
clojure -M -m clj-llm-agent.test-script
```
This script demonstrates creating an agent, interacting with it (using placeholder LLM responses), and managing context.

#### REPL Usage

You can also explore the framework interactively in a REPL:
```bash
clj # or: clojure
```
Then, require namespaces and call functions as needed. For example:
```clojure
(require '[clj-llm-agent.context :as ctx]
         '[clj-llm-agent.agent-generic :as generic]
         '[clj-llm-agent.agent-gemini]) ; Load Gemini agent specifics

(def my-ctx (ctx/make-context))
(ctx/context-set my-ctx :user "Test User")
(def my-agent (generic/make-agent :gemini-agent {:name "ClojureAgent" :context my-ctx}))
(generic/agent-converse my-agent "Hello, agent!")
```