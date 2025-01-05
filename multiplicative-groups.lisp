(in-package :mgroups)

(declaim (optimize (debug 3)))
;; (declaim (optimize (speed 3)))

(defun set-p (my-list)
  "Does the given list represent a set? It does if it has no duplicates."
  (let ((my-list-sorted (sort (copy-list my-list) #'<)))
    (= (length my-list-sorted)
       (length (delete-duplicates my-list-sorted)))))

(defun divisors (my-number)
  "Find all divisors of an integer."
  (do ((sq (isqrt my-number))
       (divisors nil)
       (d 1 (1+ d)))
      ((> d sq) (sort divisors #'<))
    (when (= 0 (mod my-number d))
      (push d divisors)
      (when (not (= d sq))
	(push (/ my-number d) divisors)))))




;; Tests.

(5am:def-suite mgroups-test-suite
  :description "Check how well multiplicative group theory functions work.")

(5am:in-suite mgroups-test-suite)

(5am:def-test check-set-p ()
  (5am:is (set-p '(1 2 3 4)))
  (5am:is (not (set-p '(1 2 3 4 2)))))

(5am:def-test check-divisors ()
  (5am:is (equal '(1 2 5 10) (divisors 10)))
  (5am:is (equal '(1 5 25) (divisors 25)))
  (5am:is (equal '(1 3 5 9 15 45) (divisors 45)))
  (5am:is (equal '(1 2 4 5 10 20 25 50 100) (divisors 100))))




(defun do-mgroups-tests ()
  (5am:run! 'mgroups-test-suite))
