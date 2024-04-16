build:
	sbcl --load 'lisp-js.asd' --eval '(ql:quickload :lisp-js)' --eval '(asdf:make :lisp-js)' --eval '(quit)'

clean:
	rm ljs.o