
(in-package :primegaps)

(defun make-prime-sieve (limit)
  (let ((sieve (make-array (1+ limit)
			   :initial-element 1
			   :element-type 'bit)))
    (setf (aref sieve 0) 0)
    (setf (aref sieve 1) 0)
    (do ((j 2 (1+ j)))
	((> j (isqrt limit)))
      (when (= 1 (aref sieve j))
	(do ((k 2 (1+ k)))
	    ((> (* k j) limit))
	  (setf (aref sieve (* k j)) 0))))
    sieve))

(fiveam:def-suite prime-sieve-suite :description "Prime sieve testing.")

(fiveam:in-suite prime-sieve-suite)

(fiveam:def-test test-primes-in-ranges ()
  (fiveam:is (= 25 (count 1 (make-prime-sieve 100)))))
