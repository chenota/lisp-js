RL_ERR_MSG=rlwrap not found
SBCL_EVAL=--eval "(ql:quickload :lisp-js)" --eval "(in-package :lisp-js)"

load: lisp-js.asd package.lisp src
ifeq (${RL_ERR_MSG},$(shell which rlwrap))
	sbcl ${SBCL_EVAL}
else
	rlwrap sbcl ${SBCL_EVAL}
endif