;;;; package.lisp

(defpackage #:cl-elemental
  (:use #:cl))

(defpackage #:prime-gaps
  (:use #:cl)
  (:export #:make-index-sieve
	   #:make-prime-sieve
	   #:prime-sieve-suite))
