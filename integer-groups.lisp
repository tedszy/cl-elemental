(in-package :integer-groups)

;; This package is for doing simple computations on small
;; integer residue groups mod n (additive or multiplicative.)
;;
;; A multiplicative resudie group mod n is denoted by Z/nZ*.
;; Take all the reduced residues mod n and strike out the
;; ones not coprime to n.
;;
;; mod 9:
;;
;; 1, 2, 3, 4, 5, 6, 7, 8 ==> 1, 2, 4, 5, 7, 8
;;
;; The result, along with the binary operation of x*y mod n
;; forms an interesting abelian group that is good for illustrating
;; ideas of group theory.
;;
;; For example we can use Z/nZ* groups to illustrage Lagrange's theorem.
;;
;; Additive Z/nZ...
;;
;; (to do) ...  docs, comments.
;;

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

(defun z/nz-residues (n)
  "Reduced residue system mod n."
  (alexandria:iota n :start 0))

(defun z/nz*-residues (n)
  "Reduced residues mod n, coprime to n, i.e., 
multiplicative residues group mod n."
  (remove-if-not #'(lambda (k) (= 1 (gcd k n))) (z/nz-residues n)))

(defun mod-binary-op-* (n)
  "Create the binary operation (x * y) mod n."
  (lambda (x y) (mod (* x y) n)))

(defun mod-binary-op-+ (n)
  "Create binary operation (x + y) mod n."
  (lambda (x y) (mod (+ x y) n)))

(defun make-cayley-table (set op)
  "Computes Cayley table for the set with binary operation op*."
  (let ((table (loop for x in set
		     collecting (loop for y in set
				      collect (funcall op x y)))))
    table))

(defun display-table (table &key (width 5))
  "Display a list of lists of integers as a table with specified field width."
  (let ((fmt (format nil "~~{~~~ad~~}~~%" width)))
    (loop for row in table
	  do (format t fmt row))))

(defun z/nz*-cayley-table (n)
  "Convenience function: get the Cayley table of Z/nZ* immediately."
  (make-cayley-table (z/nz*-residues n) (mod-binary-op-* n)))

(defun z/nz-cayley-table (n)
  "Convenience function: get the Cayley table of Z/nZ immediately."
  (make-cayley-table (z/nz-residues n) (mod-binary-op-+ n)))

;; We build an interface for interactive use with CLOS.

(defclass zgroup ()
  ((binary-op :initarg :binary-op :accessor binary-op)
   (binary-op-label :initarg :binary-op-label :accessor binary-op-label)
   (elements :initarg :elements :accessor elements)
   (id :initarg :id :accessor id)
   (order :initarg :order :accessor order)
   (cayley-table :initarg :cayley-table :accessor cayley-table))
  (:documentation "Class that can hold an integer group."))

;; (defmethod initialize-instance :after ((zg zgroup) &key)
;;   "Create the Cayley-table and find the order."
;;   (with-slots (binary-op elements cayley-table order)
;;       zg
;;     (setf cayley-table (make-cayley-table elements binary-op))
;;     (setf order (length elements))))

(defclass agroup (zgroup)
  ((modulus :initarg :modulus :accessor modulus)))

(defclass mgroup (zgroup)
  ((modulus :initarg :modulus :accessor modulus)))

(defun make-zgroup (elements binary-op id &key (binary-op-label "@"))
  (make-instance 'zgroup :binary-op binary-op
			 :elements elements
			 :id id
			 :cayley-table (make-cayley-table elements binary-op)
			 :binary-op-label binary-op-label
			 :order (length elements)))

(defun make-agroup (n)
  (let ((elements (z/nz-residues n))
	(op (mod-binary-op-+ n)))
    (make-instance 'agroup :binary-op op
			    :elements elements
			    :id 0
			    :modulus n
			    :order (length elements)
			    :cayley-table (make-cayley-table elements op)
			    :binary-op-label (format nil "MOD+~a" n))))

(defun make-mgroup (n)
  (let ((elements (z/nz*-residues n))
	(op (mod-binary-op-* n)))
    (make-instance 'mgroup :binary-op op
			    :elements elements
			    :id 1
			    :modulus n
			    :order (length elements)
			    :cayley-table (make-cayley-table elements op)
			    :binary-op-label (format nil "MOD*~a" n))))

(defmethod print-object ((zg zgroup) stream)
  (print-unreadable-object (zg stream :type nil)
    (with-slots (order binary-op-label id elements)
	zg
      (format stream "ZG ~a ~a ~d ~a" order binary-op-label id elements))))

(defmethod print-object ((ag agroup) stream)
  (print-unreadable-object (ag stream :type nil)
    (with-slots (order binary-op-label id elements)
	ag
      (format stream "AG ~a ~a ~d ~a" order binary-op-label id elements))))

(defmethod print-object ((mg mgroup) stream)
  (print-unreadable-object (mg stream :type nil)
    (with-slots (order binary-op-label id elements)
	mg
      (format stream "MG ~a ~a ~d ~a" order binary-op-label id elements))))

(defun print-cayley-table (group &key (width 5))
  (with-slots (binary-op cayley-table)
      group
    (display-table cayley-table :width width)))

(defun make-subgroup (group elements)
  (with-slots (binary-op id binary-op-label)
      group
    (make-zgroup elements binary-op id :binary-op-label binary-op-label)))

(defun has-identity? (id elements)
  (member id elements))

(defun is-closed? (op elements)
  (loop for e in elements
	always (loop for f in elements
		     always (member (funcall op e f) elements))))

(defun has-right-inverses? (op id elements)
  (loop for e in elements
	always (= 1 (loop for f in elements counting (= id (funcall op e f))))))

(defun is-subgroup? (op id elements)
  (and (has-identity? id elements)
       (is-closed? op elements)
       (has-right-inverses? op id elements)))

(defun find-subgroups (group k)
  "Find all subgroups of order k by brute force."
  (with-slots (binary-op id elements)
      group
    (let ((result nil))
      (alexandria:map-combinations
       #'(lambda (element-list)
	   (when (is-subgroup? binary-op id element-list)
	     (push (make-subgroup group element-list) result)))
       elements
       :length k)
      result)))

(defun mg-show (n &key (width 5))
  (let ((foo (make-mgroup n)))
    (print-cayley-table foo :width width)
    foo))

(defun ag-show (n &key (width 5))
  (let ((foo (make-agroup n)))
    (print-cayley-table foo :width width)
    foo))

;; Tests ==================================================================

(5am:def-suite integer-groups-test-suite
  :description "Check how well multiplicative group theory functions work.")

(5am:in-suite integer-groups-test-suite)

(5am:def-test check-set-p ()
  (5am:is (set-p '(1 2 3 4)))
  (5am:is (not (set-p '(1 2 3 4 2)))))

(5am:def-test check-divisors ()
  (5am:is (equal '(1 2 5 10) (divisors 10)))
  (5am:is (equal '(1 5 25) (divisors 25)))
  (5am:is (equal '(1 3 5 9 15 45) (divisors 45)))
  (5am:is (equal '(1 2 4 5 10 20 25 50 100) (divisors 100))))

(5am:def-test check-residues ()
  (5am:is (equal '(0 1 2 3 4 5 6 7 8) (z/nz-residues 9)))
  (5am:is (equal '(1 2 4 5 7 8) (z/nz*-residues 9))))

(5am:def-test group-mod-binary-op ()
  (let ((mod7* (mod-binary-op-* 7))
	(mod15* (mod-binary-op-* 15))
	(mod7+ (mod-binary-op-+ 7))
	(mod15+ (mod-binary-op-+ 15))
	(x 199) (y 101))
    (5am:is (= 2 (funcall mod7* x y)))
    (5am:is (= 14 (funcall mod15* x y)))
    (5am:is (= 6 (funcall mod7+ x y)))
    (5am:is (= 0 (funcall mod15+ x y)))))

;; Just run this to run the tests.

(defun do-integer-groups-tests ()
  (5am:run! 'integer-groups-test-suite))
