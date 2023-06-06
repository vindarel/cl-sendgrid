(defpackage sendgrid
  (:use :cl)
  (:export #:send-email
           #:*api-key-environment-variable-name*
           #:*verbose*))
(in-package :sendgrid)

;;; Send an email with SendGrid's API.

(defparameter *sendgrid-api* "https://api.sendgrid.com/v3/mail/send")

(defvar *api-key-environment-variable-name*
  "SENDGRID_API_KEY")

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
        "email": "test@example.com",
        "name": "Test Name"
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

(defun sendgrid-json (&key
                        to
                        from-email
			from-name
                        reply-to
                        subject
                        (content-type "text/plain") ; this duplication is a-must. &rest doesn't pass the default value of caller's keys.
                        content-value
                      &allow-other-keys)
  "Build the data json.
  `to': one email address or a list.
  `reply-to': a pair of email and name."
  (assert (and to
               from-email
               subject
               content-value))
  (unless (or (null reply-to)
              (and (stringp (cdr (assoc "email"
                                        reply-to
                                        :test #'string=)))
                   (stringp (cdr (assoc "name"
                                        reply-to
                                        :test #'string=)))))
    (error "\"reply-to\" must be an alist pair as ((\"email\" . string) (\"name\" . string))"))
  (let* ((to (ensure-list to))
         (json-alist
           (append `(("personalizations"
                      ,(loop for dest in to
                             collect `("to" (("email" . ,dest))))))
		   `(("from" ("email" . ,from-email)
			     ("name" . ,from-name)))
                   (when reply-to
                     `(,(cons "reply_to" reply-to)))
                   `(("subject" . ,subject)
                     ("content" (("type" . ,content-type)
                                 ("value" . ,content-value)))))))
    (jonathan:to-json json-alist :from :alist)))

;; test:
#+nil
(progn
  ;; Base case:
  (assert (string-equal (sendgrid-json :to "to@mail" :from "me@mail" :subject "hello" :content-value "yo" :reply-to '(("email" . "@") ("name" . "me")))
                        "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"reply_to\":{\"email\":\"@\",\"name\":\"me\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}"))

  ;; With two receivers:
  (assert (string-equal (sendgrid-json :to '("to@mail" "to-two@mail") :from "me@mail" :subject "hello" :content-value "yo" :reply-to '(("email" . "@") ("name" . "me")))
                        "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}],\"to\":[{\"email\":\"to-two@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"reply_to\":{\"email\":\"@\",\"name\":\"me\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}")))

(defun send-email (&rest rest
                   &key
                     to
                     from-email
		     from-name
                     subject
                     content
                     reply-to
                     (content-type "text/plain")
                     (api-key (uiop:getenv *api-key-environment-variable-name*))
                   &allow-other-keys) ; &allow-other-keys can help gradual API updates.
  "Send an email with SendGrid's API. https://docs.sendgrid.com/api-reference/mail-send/mail-send#body Currently only supporting basic parameters for 80% use cases.

 Notice that `reply-to', if not nil, should be an alist as ((\"email\" . string) (\"name\" . string))"
  (dex:post *sendgrid-api*
            :headers `(("Authorization" . ,(concatenate
                                            'string
                                            "Bearer "
                                            api-key))
                       ("content-Type" . "application/json"))
            :verbose *verbose*
            :content (apply #'sendgrid-json
                            (append `(:content-value ,content)
                                    rest)))) ; The compiler might warn variables defined but never used. But keeping the warnings should be better for future code modification. E.g., suppressing the warnings by (declare (ignorable ...) could result in debugging difficulties once the API changes.

