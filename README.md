# Send emails with the SendGrid API

https://sendgrid.com/

# Installation
On Quicklisp (2020-10):

    (ql:quickload "sendgrid")

and on [Ultralisp](https://ultralisp.org/).

# Current usage (Please jump to [Updated usage](#updated-usage) to see upcoming changes)
Create a SendGrid account and set these variables:

```lisp
(setf *email-config*
  '(:|api-key| "your private api key"
    :|from| "the default 'from' address."))
```

Send an email with `send-email`:

```lisp
(sendgrid:send-email &key to
                          (from (getf *email-config* :|from|))
                          (reply-to (getf *email-config* :|reply-to|))
                          subject
                          content
                          (verbose *verbose*))

;; #()
;; 202
;; #<HASH-TABLE :TEST EQUAL :COUNT 10 {10079655A3}>
;; #<QURI.URI.HTTP:URI-HTTPS https://api.sendgrid.com/v3/mail/send>
;; #<CL+SSL::SSL-STREAM for #<FD-STREAM for "socket 192.168.1.11:36670, peer: 159.122.xxx.yyy:443" {1007880183}>>
```

It takes the time of a POST request.

# Updated usage
## API KEY
After obtaining an API key from [SendGrid](https://sendgrid.com/), you can either 
### Provide the API key via an Operating System environment variable
```lisp
CL-USER> sendgrid:*api-key-environment-variable-name*
"SENDGRID_API_KEY" ; default environment variable name, you can change it.
```
Then you can do it by OS built-in facility, such as
```bash
export SENDGRID_API_KEY=your_sendgrid_api_key_value
```
Or set it using UIOP
```lisp
CL-USER> (setf (uiop:getenv sendgrid:*api-key-environment-variable-name*) your-api-key-value)
```
### Provide the API key when calling `sendgrid:send-mail`
`api-key` is one of the parameter of `sendgrid:send-mail`
```lisp
(sendgrid:send-email &rest rest
                     &key
                     to
                     from
                     subject
                     content
                     reply-to ; if not nil, should be an alist as (("email" . string) ("name" . string))
                     (content-type "text/plain")
                     (api-key (uiop:getenv *api-key-environment-variable-name*))
                   &allow-other-keys)
```
## Sample plain text email
### Minimum
```lisp
(sendgrid:send-email :to "recipient@example.com"
                     :from "noreply@example.com"
                     :subject "Sending emails from SendGrid is fun!"
                     :content "Sending emails from SendGrid is fun!")
```
### With `reply-to`
```lisp
(sendgrid:send-email :to "recipient@example.com"
                     :from "team@example.com"
                     :reply-to '(("email" . "noreply@example.com") ("name" . "No Reply"))
                     :subject "Sending emails from SendGrid is fun!"
                     :content "Sending emails from SendGrid is fun!")
```
## Sample HTML email
```lisp
(sendgrid:send-email :to "recipient@example.com"
                     :from "noreply@example.com"
                     :subject "Sending emails from SendGrid is fun!"
                     :content-type "text/html"
                     :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>")
```
## Verbose mode example
```
(let ((sendgrid:*verbose* t))
  (sendgrid:send-email :to "recipient@example.com"
                       :from "noreply@example.com"
                       :subject "Sending emails from SendGrid is fun!"
                       :content-type "text/html"
                       :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>"))
```

## Optional - Add sender name

Without a from name, your sending email will be the name of the email. Use the optional `:from-name` key to add a more appropriate sender name.

```lisp
(sendgrid:send-email :to "recipient@example.com"
	              :from "noreply@example.com"
		      :from-name "Jane Doe"
		      :subject "Sending emails from SendGrid is fun!"
		      :content-type "text/html"
		      :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>")
```
## Email scheduling - :send-at key

The :send-at key takes a unix epoch time and sends your email at that specific time.
 
Free sendgrid accounts (at this time - 4th Oct 23) are limited to sending up to 3 days in the future. Therefore the `:send-at` key is limited to sending emails between 1-3 days in the future.

### Send email now

To send an email now, you can omit the `:send-at` key (see examples above).

### Send email in the future

Use the function `(now-plus-n-days n)` where `n` can be 1, 2, or 3 days in the future. This function will return the epoch time for that future date.

```lisp
(sendgrid:send-email :to "recipient@example.com"
	              :from "noreply@example.com"
		      :from-name "Jane Doe"
		      ;; send email 2 days in the future
		      :send-at (now-plus-n-days 2)
		      :subject "Sending emails from SendGrid is fun!"
		      :content-type "text/html"
		      :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>")
```

## Sending emails with .txt attachments

You can use the `:attachments` key to send attachments. Set attachments to `t` and then you can use the `:file` and `:filename` keys to provide your attachement.

`:file` takes the path to the txt file you want to attach. 
`:filename` takes a string that represents the file name of the attachment.

Example: 

```lisp
(sendgrid:send-email :to "recipient@example.com"
	              :from "noreply@example.com"
		      :from-name "Jane Doe"
		      :send-at (now-plus-n-days 2)
		      :subject "Sending emails from SendGrid is fun!"
		      :content-type "text/html"
		      :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>"
		      :attachments t
		      :file "/path/to/txt/file"
		      :filename "filename.txt")
```

## Sending emails with `pdf` attachments

Ensure that the `:file` is a path to your pdf file and the `:filename` ends with `.pdf`.


Example: 

```lisp
(sendgrid:send-email :to "recipient@example.com"
	              :from "noreply@example.com"
		      :from-name "Jane Doe"
		      :send-at (now-plus-n-days 2)
		      :subject "Sending emails from SendGrid is fun!"
		      :content-type "text/html" ;; this is the content type of the email, not the attachments.
		      :content "<h1>A title</h1><br/><strong>Sending emails from SendGrid is fun!</strong>"
		      :attachments t
		      :file "/path/to/txt/pdf/file"
		      :filename "filename.pdf")
```


You will receive an email with the attachment `filename.pdf`. The content of the file will match that of `/path/to/pdf/file`.


## Triggering email automations with add-contact-to-list

Inside sendgrid, if you navigate to Marketing --> Contacts, you can create a contact list. A contact list is a specific segment of people that did, or will do a specific action. For example, you can have a contact list for new subscribers to your product.

A list can have automatic actions attached to it. For example, you can trigger an email automation based on somebody subscribing to the list (This is managed in Marketing --> Automations).

With the `add-contact-to-list` function, you can add a single contact to a specifc contact list by using the list-id. 

Note - you can find the list-id at the end of the contact list url. For example `https://mc.sendgrid.com/contacts/lists/ac32607e-e253-490b-a171-d84d88xxxxxx`, in this case `ac32607e-e253-490b-a171-d84d88xxxxxx` is the list id.

```lisp
(add-contact-to-list :email "vinn@kev.com"
                     :firstname "Vinn"
                     :list-id "ac32607e-e253-490b-a171-d84d88xxxxxx")
```

# See also

* https://github.com/40ants/mailgun (Mailgun: just a bit more overhead to getting started, a free plan a bit less free)
* https://github.com/CodyReichert/awesome-cl#email
* [spinneret](https://github.com/ruricolist/spinneret) (recommended) or [cl-who](https://edicl.github.io/cl-who/) for generating HTML strings.
# Licence

MIT.
