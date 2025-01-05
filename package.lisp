;;;; package.lisp

(defpackage #:cl-elemental
  (:use #:cl))

(defpackage #:prime-gaps
  (:use #:cl)
  (:export #:make-prime-sieve
	   #:do-prime-gaps-tests))

(defpackage #:mgroups
  (:use #:cl))
