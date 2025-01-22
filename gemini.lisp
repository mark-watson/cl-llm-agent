(in-package :cl-llm-agent-gemini)

(defvar *gemini-api-key* nil
  "Your Google Gemini API key. Set this before using Gemini functions.")

(defvar *gemini-base-url* "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro-flash:generateContent"
  "Base URL for Gemini 2.0 Flash API.")

(defun gemini-generate-content (prompt &key (api-key *gemini-api-key*))
  "Calls the Gemini 2.0 Flash API to generate content based on the prompt."
  (unless api-key
      (error "Gemini API key is not set. Set cl-llm-agent-gemini:*gemini-api-key*."))

  (let ((url (format nil "~A?key=~A" *gemini-base-url* api-key))
        (data (list :contents (list (list :parts (list (list :text prompt)))))))
    (handler-case
        (let* ((response-str (http-post-json url data))
               (response-json (parse-json response-str)))
          (if (getf response-json :error)
              (error "Gemini API Error: ~A" (getf response-json :error))
              (let ((candidates (getf response-json :candidates)))
                (if candidates
                    (let ((content (getf (first candidates) :content)))
                      (if content
                          (let ((parts (getf content :parts)))
                            (if parts
                                (getf (first parts) :text)
                                (error "Gemini response structure error: No parts in content.")))
                          (error "Gemini response structure error: No content in candidate.")))
                    (error "Gemini response structure error: No candidates in response."))))))
      (error (e)
        (error "Error communicating with Gemini API: ~A" e))))

