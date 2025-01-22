(in-package :cl-llm-agent-tools)

(defvar *tool-registry* (make-hash-table :test #'equal)
  "Registry to store available tools.")

(defmacro define-tool (name description parameters &body body)
  "Defines a tool with a name, description, parameters, and implementation."
  `(register-tool ',name
                  :description ,description
                  :parameters ',parameters
                  :function (lambda ,parameters
                              ,@body)))

(defun register-tool (name &key description parameters function)
  "Registers a tool in the tool registry."
  (setf (gethash name *tool-registry*)
        (list :name name :description description :parameters parameters :function function)))

(defun execute-tool (tool-name arguments)
  "Executes a registered tool with the given arguments."
  (let ((tool-data (gethash tool-name *tool-registry*)))
    (if tool-data
        (let ((tool-function (getf tool-data :function))
              (tool-parameters (getf tool-data :parameters)))
          (if tool-function
              (if (= (length arguments) (length tool-parameters)) ; Basic parameter count check
                  (apply tool-function arguments)
                  (error "Incorrect number of arguments for tool ~A. Expected ~A, got ~A."
                         tool-name (length tool-parameters) (length arguments)))
              (error "Tool ~A has no function defined." tool-name)))
        (error "Tool ~A not found." tool-name))))

(defun list-tools ()
  "Returns a list of registered tool names and descriptions."
  (loop for tool-data being the hash-value of *tool-registry*
        collect (list :name (getf tool-data :name)
                      :description (getf tool-data :description))))


;; --- Predefined Tools ---

(define-tool tool-read-directory "Reads the contents of a directory."
  (directory-path)
  "directory-path (string): The path to the directory."
  (let ((dir-path (namestring (truename directory-path)))) ; Ensure absolute path
    (if (probe-directory dir-path)
        (mapcar #'namestring (directory (concatenate 'string dir-path "/*.*")))
        (format nil "Directory not found: ~A" directory-path))))

(define-tool tool-read-file "Reads the contents of a file."
  (file-path)
  "file-path (string): The path to the file."
  (let ((file-path (namestring (truename file-path)))) ; Ensure absolute path
    (if (probe-file file-path)
        (with-open-file (stream file-path :direction :input :if-does-not-exist nil)
          (when stream
            (loop for line = (read-line stream nil nil)
                  while line
                  collect line)))
        (format nil "File not found: ~A" file-path))))

