(in-package :cl-llm-agent-tools)

(defvar *tool-registry* (make-hash-table :test #'equal)
  "Registry to store available tools.")

(defun directory-pathname-p (pathname)
  (and (pathnamep pathname)
       (or (null (pathname-name pathname))
           (eq (pathname-name pathname) :unspecific))
       (or (null (pathname-type pathname))
           (eq (pathname-type pathname) :unspecific))))

(defun probe-directory (pathname)
  (and (probe-file pathname)
       (directory-pathname-p pathname)))

(defmacro define-tool (name description parameters parameter-example a-function)
  "Defines a tool with a name, description, parameters, and implementation."
  `(register-tool ,name
                  :description ,description
                  :parameters ',parameters
                  :parameter-example ,parameter-example
                  :function ,a-function))

(defun register-tool (name &key description parameters parameter-example function)
  "Registers a tool in the tool registry."
  (setf (gethash name *tool-registry*)
        (list :name name :description description :parameters parameters
	      :parameter-example parameter-example :function function)))

(defun execute-tool (tool-name arguments)
  "Executes a registered tool with the given arguments."
  (format t "* tools.lisp: execute-tool ~A~%" tool-name)
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
                      :description (getf tool-data :description)
                      :function (getf tool-data :function))))


;; --- Predefined Tools ---

(defun helper-tool-read-directory (directory-path)
  (format t "* helper-tool-read-directory ~A~%" directory-path)
  (let ((dp (or directory-path ".")))
    (let ((dir-path (truename dp))) ; Ensure absolute path
      (format t "  dir-path = ~A~%" dir-path)
      (if (probe-directory dir-path)
	  (let ((flist
		  (remove-if 
		   (lambda (path)
		     (let ((name (file-namestring path)))
		       (or (char= (char name 0) #\#)         ; starts with #
			   (char= (char (reverse name) 0) #\~)))) ; ends with ~
		   (mapcar #'file-namestring
			   (uiop:directory-files dir-path)))))
	    (print flist)
            (mapcar #'namestring flist))
	  (format nil "Directory not found: ~A" dir-path)))))

(define-tool "tool-read-directory" "Reads the contents of a directory."
  (directory-path)
  "directory-path (string): The path to the directory."
  (lambda (&aux directory-path)
    (format t "* tool-read-directory ~A~%" directory-path)
    (helper-tool-read-directory directory-path)))


(defun helper-tool-read-file (directory-path)
  (let ((dir-path (or directory-path ".")))
    (format t "* helper-tool-read-file in directory ~A~%" dir-path)
    (let ((dir-path-s (namestring (truename dir-path))))
      (format t "    dir-path = ~A~%" dir-path-s)
      (if (probe-directory dir-path-s)
          (mapcar #'namestring (directory (concatenate 'string dir-path-s "/*.*")))
        (format nil "Directory not found: ~A" dir-path-s)))))

(define-tool "tool-read-file" "Reads the contents of a file."
  (file-path)
  "file-path (string): The path to the file."
  (lambda (directory-path)
    (format t "* tool-read-file in directory ~A~%" dir-path)
    (helper-tool-read-file directory-path)))


(define-tool "tool-search-web" "Search the web."
  (query)
  "query: web search query."
  (lambda (query)
    (format t "* tool-search-web query: ~A~%" query)
    (cl-llm-agent-tavily:tavily-search query)))
