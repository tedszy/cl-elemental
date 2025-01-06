(in-package :mgroups)

(declaim (optimize (debug 3)))
;; (declaim (optimize (speed 3)))

(defun set-p (my-list)
  "Does the given list represent a set? It does if it has no duplicates."
  (let ((my-list-sorted (sort (copy-list my-list) #'<)))
    (alexandria:length= my-list-sorted
			(delete-duplicates my-list-sorted))))

(defun divisors (my-number)
  "Compute list of all divisors of positive integer my-number."
  (do ((sq (isqrt my-number))
       (divisors nil)
       (d 1 (1+ d)))
      ((> d sq) (sort divisors #'<))
    (when (= 0 (mod my-number d))
      (push d divisors)
      (when (not (= d sq))
	(push (/ my-number d) divisors)))))

(defun z/nz (n)
  "Reduced residues mod n."
  (alexandria:iota (1- n) :start 1))

(defun z/nz* (n)
  "Reduced residues mod n, coprime to n, i.e., multiplicative residue group mod n."
  (remove-if-not #'(lambda (k) (= 1 (gcd k n))) (z/nz n)))

(defun mod-multiplication (n)
  "Group multiplication X * Y mod n."
  (lambda (x y) (mod (* x y) n)))

(defun cayley-table (residue-set modn*)
  "Computes Cayley table for the set of residues with multiplication operation modn*."
  (let ((table (loop for x in residue-set
		     collecting (loop for y in residue-set
				      collect (funcall modn* x y)))))
    table))

(defun display-table (table &key (width 5))
  (let ((fmt (format nil "~~{~~~ad~~}~~%" width)))
    (loop for row in table
	  do (format t fmt row))))

(defun z/nz*-cayley-table (n &key (display nil) (width 5))
  (let ((ct (cayley-table (z/nz* n) (mod-multiplication n))))
    (if display
	(display-table ct :width width)
	ct)))





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

(5am:def-test check-residues ()
  (5am:is (equal '(1 2 3 4 5 6 7 8) (z/nz 9)))
  (5am:is (equal '(1 2 4 5 7 8) (z/nz* 9))))

(5am:def-test group-multiplication ()
  (let ((mod7* (mod-multiplication 7))
	(mod15* (mod-multiplication 15)))
    (5am:is (= 2 (funcall mod7* 199 101)))
    (5am:is (= 14 (funcall mod15* 199 101)))))




(defun do-mgroups-tests ()
  (5am:run! 'mgroups-test-suite))
