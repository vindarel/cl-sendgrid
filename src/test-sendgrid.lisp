
(defparameter *email-config* nil)

;; Fill config-test.lisp with your credential data.
(load "config-test.lisp")

(format t "Sending test email with config: ~a~&" *email-config*)

(format t "Continue? [type any key to continue]~&")

(handler-case
    (read)
  #+sbcl
  (sb-sys:interactive-interrupt ()
    (format t "Aborting.~&")
    (uiop:quit 1)))

(sendgrid:send-email :api-key (getf *email-config* :|api-key|)
                     :to (getf *email-config* :|to|)
                     :from (getf *email-config* :|from|)
                     :from-name (getf *email-config* :|from-name|)
                     :subject "Sending emails from SendGrid is fun!"
                     :content-type "text/html"
                     :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>"
                     )


(uiop:quit)
