;; Example sendgrid config.
(setf *email-config*
      '(:|api-key| "SG.xyzSecretxyz"
        :|from| "you+test@mail.com"  ;; the email registered on Sendgrid.
        :|to| "you@mail.com"
        :|reply-to| ("you+test@mail.com" "Me (sendgrid)")))
