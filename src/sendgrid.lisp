(defpackage sendgrid
  (:use :cl)
  (:export #:send-email
           #:*api-key-environment-variable-name*
           #:*verbose*
	   #:now-plus-n-days))
(in-package :sendgrid)

;;; Send an email with SendGrid's API.

(defparameter *sendgrid-api* "https://api.sendgrid.com/v3/mail/send")

(defvar *api-key-environment-variable-name*
  "SENDGRID_API_KEY")

(defvar *verbose* nil)

(defparameter *one-day-in-seconds* 85400 "one day less 1000 seconds")

(defun now()
  "Unix time now"
  (local-time:timestamp-to-unix (local-time:now)))

(defun now-plus-n-days(days)
  "Sendgrid api allows you to send emails up to 4 days in the future

Specify  n days in the future where n is either 1, 2, 3 or 4 days.

A unix time for that date will be returned. If any other number of days is provided, the now function will return."
  (if (and (> days 0) (< days 5))
      (+ (now) (* days *one-day-in-seconds*))
      (warn "Please pass in 1-4 days. Sendgrid only allows future sending of emails between 1-4 days.")))

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
                        from
			from-name
                        reply-to
                        subject
			send-at
                        (content-type "text/plain") ; this duplication is a-must. &rest doesn't pass the default value of caller's keys.
                        content-value
                      &allow-other-keys)
  "Build the data json.
  `to': one email address or a list.
  `reply-to': a pair of email and name.
  `from': the sending email
  `from-name': the sending name that shows up in the inbox"
  (assert (and to
               from
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
                             collect `("to" (("email" . ,dest)))))
		     ,(if (null from-name)
			 `("from" ("email" . ,from))
			 `("from" ("email" . ,from)
				  ("name" . ,from-name))))
                   (when reply-to
                     `(,(cons "reply_to" reply-to)))
                   `(("subject" . ,subject)
		     ("send_at" . ,send-at)
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
                     from
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
