# Send emails with the SendGrid API

https://sendgrid.com/

Create an account an set these variables:

```lisp
(setf *email-config*
  '(:|api-key| "your private api key"
    :|from| "the default 'from' address."))
```

Send an email with `send-email`:

```lisp
sendgrid:send-email &key to (from (getf *email-config* :|from|)) subject content (verbose *verbose*)
```

It takes the time of a POST request.

TODO:

- [X] use the code
- [ ] actually use the .asd
- [ ] make the "to" parameter accept a list of addresses.

# See also

* https://github.com/40ants/mailgun (Mailgun: just a bit more overhead to getting started, a free plan a bit less free)
* https://github.com/CodyReichert/awesome-cl#email

# Licence

MIT.
