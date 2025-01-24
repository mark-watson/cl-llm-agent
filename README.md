# cl-llm-agent

## example

CL-USER 2 > (load "test.lisp")
; Loading text file /Users/markw/quicklisp/local-projects/cl-llm-agent/test.lisp

LLM Response: I need more information to fulfill this request.  My current task and location are not available in the context.  Please provide my current task and location.


Current Task from Context: researching restaurants

LLM Response: Okay, I'll refine the restaurant search to only include Italian restaurants.  I need more information to do this effectively.  Specifically, I need a directory or file containing restaurant information, including their cuisine type.  Please provide me with the path to such a file or directory.  I will then use the appropriate tool to filter the results.

## debugging

```
CL-LLM-AGENT 10 > (format nil "Available tools:~%~{~S: ~%~}" (loop for tool in (list-tools) collect (list (getf tool :name) (getf tool :description))))
"Available tools:
(TOOL-SEARCH-WEB \"Search the web.\"): 
(TOOL-READ-DIRECTORY \"Reads the contents of a directory.\"): 
(TOOL-READ-FILE \"Reads the contents of a file.\"): 
"

```
