(in-package :cl-llm-agent-utils)

(defun parse-json (json-string)
  "Parses a JSON string into a Lisp data structure."
  (cl-json:decode-json-from-string json-string))

(defun pp-hash (message h)
  "Prints a message followed by the contents of a hash table."
  (format t "~A~%" message)
  (when (hash-table-p h)
    (loop for key being the hash-key of h
          using (hash-value value)
          do (format t "  ~A: ~A~%" key value)))
  (unless (hash-table-p h)
    (format t "  Not a hash table: ~A~%" h)))