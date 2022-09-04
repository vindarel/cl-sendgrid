(defpackage sendgrid
  (:use :cl)
  (:export #:send-email
           #:*api-key-environment-variable-name*
           #:*verbose*))
(in-package :sendgrid)

;;; Send an email with SendGrid's API.

(defparameter *api-key-environment-variable-name*
  "SENDGRID_API_KEY")

(defparameter *sendgrid-api* "https://api.sendgrid.com/v3/mail/send")

(defvar *verbose* nil)

#|
The JSON looks like:
{
    "content": [
        {
            "type": "text/plain",
            "value": "and easy to do anywhere, even with cURL"
        }
    ],
    "from": {
        "email": "test@example.com"
    },
    "reply_to": {
        "email": "sam.smith@example.com",
        "name": "Sam Smith"
    },
    "personalizations": [
        {
            "to": [
                {
                    "email": "test@example.com",
                    "name": "John Doe"
                }
            ]
        }
    ],
    "subject": "Sending with SendGrid is Fun"
}
|#

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST.
   (from Alexandria)"
  (if (listp list)
      list
      (list list)))

(defun sendgrid-json (&key to from reply-to subject content-type content-value)
  "Build the data json.
  `to': one email address or a list.
  `reply-to': a pair of email and name."
  (unless (or nil (consp reply-to))
    (error "\"reply-to\" must be a pair with an email and a name (strings)."))
  (setf to (ensure-list to))
  (let ((json-alist
         `(("personalizations"
            ,(loop for dest in to
                collect `("to" (("email" . ,dest)))))
           ("from" ("email" . ,from))
           ("reply_to" ("email" . ,(car reply-to))
                       ("name" . ,(cadr reply-to)))
           ("subject" . ,subject)
           ("content" (("type" . ,content-type)
                       ("value" . ,content-value))))))
    (jonathan:to-json json-alist :from :alist)))

;; test:
#+nil
(progn
  ;; Base case:
  (assert (string-equal (sendgrid-json :to "to@mail" :from "me@mail" :subject "hello" :content-value "yo" :reply-to '("@" "me"))
                        "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"reply_to\":{\"email\":\"@\",\"name\":\"me\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}"))

  ;; With two receivers:
  (assert (string-equal (sendgrid-json :to '("to@mail" "to-two@mail") :from "me@mail" :subject "hello" :content-value "yo" :reply-to '("@" "me"))
                        "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}],\"to\":[{\"email\":\"to-two@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"reply_to\":{\"email\":\"@\",\"name\":\"me\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}")))


(defun send-email (&key
                     to
                     (from (getf *email-config* :|from|))
                     (reply-to (getf *email-config* :|reply-to|))
                     subject
                     (content-type "text/plain")
                     content
  "Send an email with SendGrid's API.

  -`from': from the `*email-config*' by default.
  - `reply-to': must be a list with an email address and a name.
                     (api-key (uiop:getenv *api-key-environment-variable-name*))
                   &allow-other-keys) ; &allow-other-keys can help gradual API updates.

  todo: make `to' accept multiple addresses."
  (assert (and to from subject content))
  (dex:post *sendgrid-api*
            :headers `(("Authorization" . ,(concatenate
                                            'string
                                            "Bearer "
                                            api-key))
                       ("content-Type" . "application/json"))
            :content (sendgrid-json :to to
                                    :from from
                                    :reply-to reply-to
                                    :subject subject
                                    :content-value content
                                    :content-type content-type)))
            :verbose *verbose*
