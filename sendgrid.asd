(asdf:defsystem "sendgrid"
  :version "0.1"
  :author "vindarel"
  :license "MIT"
  :depends-on (:dexador
               :jonathan
	       :qbase64
	       :alexandria)
  :components ((:module "src"
                :components
                ((:file "sendgrid"))))

  :description "Wrapper to SendGrid's API."
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  )
