;;;; package.lisp

(defpackage #:cl-elemental
  (:use #:cl))

(defpackage #:prime-gaps
  (:use #:cl)
  (:export #:make-prime-sieve
	   #:do-prime-sieve-tests))
