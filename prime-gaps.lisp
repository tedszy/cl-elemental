(in-package :prime-gaps)

;; Set this to t if you want the tests to run on compile/load.
(setf fiveam:*run-test-when-defined* nil)

;; WHen using make-array with element-type bit, SBCL will create
;; a bitvector. This is a great saving of space. Sieves going to
;; an upper-limit of 3 billion are easy now. Bit 0 means composite
;; and bit 1 means prime.
;;
;; All sieve elements are initially marked with bit 1 (prime).
;;
;; 
;; 0 and 1 are not prime so they are marked with bit 0 immediately
;; after make-array.
;;
;; Prime-sieve upper-limit defaults to something fast: 100,000.
;;
;; upper-limit of 1000 means the prime sieve covers the interval
;; [1,1000] precisely, including both 1 and 1000.
;;
;;This means that...
;;
;; (make-prime-sieve 1000) ==> 1001 elements: 0 and [1,1000] inclusive.

(defun make-prime-sieve (&optional (upper-limit 100000))
  (let ((sieve (make-array (1+ upper-limit)
			   :initial-element 1
			   :element-type 'bit)))
    (setf (aref sieve 0) 0)
    (setf (aref sieve 1) 0)
    (do ((j 2 (1+ j)))
	((> j (isqrt upper-limit)))
      (when (= 1 (aref sieve j))
	(do ((k 2 (1+ k)))
	    ((> (* k j) upper-limit))
	  (setf (aref sieve (* k j)) 0))))
    sieve))

;; Tests.

(fiveam:def-suite prime-sieve-test-suite
  :description "Prime sieve testing.")

(fiveam:in-suite prime-sieve-test-suite)

(fiveam:def-test check-limits ()
  (let ((sieve (make-prime-sieve 100)))
    (fiveam:is (= 101 (length sieve)))))

(fiveam:def-test check-indivitual-primes ()
  (let ((sieve (make-prime-sieve 1000)))
    (fiveam:is (= 1 (aref sieve 641)))
    (fiveam:is (= 0 (aref sieve 707)))))

(fiveam:def-test check-primes-in-ranges ()
  (fiveam:is (= 25 (count 1 (make-prime-sieve 100))))
  (fiveam:is (= 26 (count 1 (make-prime-sieve 101))))
  (fiveam:is (= 27 (count 1 (make-prime-sieve 103))))
  (fiveam:is (= 168 (count 1 (make-prime-sieve 1000))))
  (fiveam:is (= 169 (count 1 (make-prime-sieve 1009)))))

;; Usage:
;; (ql:quickload :cl-elemental)
;; (primegaps:do-prime-gaps-tests)
(defun do-prime-gaps-tests ()
  (fiveam:run! 'prime-sieve-test-suite))


