LISP := sbcl

test:
	$(LISP) --disable-debugger \
		--load sendgrid.asd \
		--eval "(ql:quickload :sendgrid)" \
		--load src/test-sendgrid.lisp
