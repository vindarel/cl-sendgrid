# Send emails with the SendGrid API

https://sendgrid.com/

On Quicklisp (2020-10):

    (ql:quickload "sendgrid")

and on [Ultralisp](https://ultralisp.org/).

Create an account an set these variables:

```lisp
(setf *email-config*
  '(:|api-key| "your private api key"
    :|from| "the default 'from' address."))
```

Send an email with `send-email`:

```lisp
(sendgrid:send-email &key to
                          (from (getf *email-config* :|from|))
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

TODO:

- [ ] make the "to" parameter accept a list of addresses.
- [ ] add "reply_to"

# See also

* https://github.com/40ants/mailgun (Mailgun: just a bit more overhead to getting started, a free plan a bit less free)
* https://github.com/CodyReichert/awesome-cl#email

# Licence

MIT.
