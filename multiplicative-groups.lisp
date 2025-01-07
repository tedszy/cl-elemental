(in-package :mgroups)

;; This package is for doing simple computations on small
;; multiplicative residue groups mod n.
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
;; For convenience/comparison we add ability to create
;; and work with (Z/nZ, modn+) groups as well. 

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
  "Reduced residues mod n."
  (alexandria:iota (1- n) :start 1))

(defun z/nz*-residues (n)
  "Reduced residues mod n, coprime to n, i.e., multiplicative residue group mod n."
  (remove-if-not #'(lambda (k) (= 1 (gcd k n))) (z/nz-residues n)))

(defun mod-binary-op-* (n)
  "Create the binary operation (x * y) mod n."
  (lambda (x y) (mod (* x y) n)))

(defun mod-binary-op-+ (n)
  "Create binary operation (x + y) mod n."
  (lambda (x y) (mod (+ x y) n)))

(defun cayley-table (set op)
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
  (cayley-table (z/nz*-residues n) (mod-binary-op-* n)))

(defun z/nz-cayley-table (n)
  "Convenience function: get the Cayley table of Z/nZ immediately."
  (cayley-table (z/nz-residues n) (mod-binary-op-+ n)))



;; We build a nice interface for interactive use with CLOS.
;; An MGROOP instance is an object with a set of elements
;; and a binary operation.
;;
;; These objects are initialized by passing a modulus
;; to initialize-instance.

(defclass mgroup ()
  ((modulus :initarg :modulus :accessor modulus)
   (elements :initarg :elements :accessor elements)
   (binary-op :accessor binary-op)
   (order :initarg :order :accessor order))
  (:documentation "Multiplicative group mod m."))

;; The binary operation is automatically computed
;; from the modulus.

(defmethod initialize-instance :after ((obj mgroup) &key)
  "Get the modulus and create a binary operation with it."
  (with-slots (modulus binary-op)
      obj
    (setf binary-op (mod-binary-op-* modulus))))

(defmethod print-object ((obj mgroup) stream)
  (print-unreadable-object (obj stream :type nil)
    (with-accessors ((modulus modulus)
		     (elements elements)
		     (order order))
	obj
      (format stream "O:~a M*~a ~a" order modulus elements))))

;; Constructor function.

(defun make-z/nz*-group (n)
  "Constructs a Z/nZ* group given modulus n."
  (let ((elements (z/nz*-residues n)))
    (make-instance 'mgroup :modulus n
			   :elements elements
			   :order (length elements))))

;; Generic interfaces.

(defgeneric make-subgroup (mgroup elements)
  (:documentation "returns mgroup where elements are subset of mgroup's elements
with the same binary operation."))

(defgeneric compute-cayley-table (obj)
  (:documentation "Compute the Cayley table of the group obj."))

(defgeneric print-cayley-table (obj &key)
  (:documentation "Prints out Cayley table with specified field width."))

(defgeneric find-subgroups (obj order)
  (:documentation "Find subgroups of a group having give order."))

;; Methods.

(defmethod compute-cayley-table ((mg mgroup))
  "Returns a Cayley table as a list of lists."
  (cayley-table (elements mg) (binary-op mg)))

(defmethod print-cayley-table ((mg mgroup) &key (width 5))
  "Display Cayley table to standard output."
  (display-table (cayley-table (elements mg) (binary-op mg)) :width width))

(defmethod make-subgroup ((mg mgroup) elements)
  "Mgroups are not always the full Z/nZ*. A smaller subset
of the elements may also be a group under the same binary operation.
And so we need a way to create mgroups from arbitrary element sets."
  (make-instance 'mgroup :modulus (modulus mg)
			 :elements elements
			 :order (length elements)))

;; Test if a set of elements and a binary operation is a group.

(defun has-1-p (set)
  "Mgroups have to have 1, the identity."
  (member 1 set))

(defun has-closure-p (set op)
  "All the elements in the Cayley table must be elements of the given set."
  (loop for e in set
	always (loop for f in set
		     always (member (funcall op e f) set))))

;; Since mgroups are Abelian, we only need to check for right inverses.

(defun has-right-inverses-p (set op)
  "Every element in the set must have exactly one right inverse."
  (loop for e in set
	always (= 1 (loop for f in set counting (= 1 (funcall op e f))))))

(defun is-group-p (set op)
  "Does <set, op> form a group?"
  (and (has-1-p set)
       (has-closure-p set op)
       (has-right-inverses-p set op)))

;; This can be made faster by using the following idea:
;; we know 1 must be in any subgroup, so we remove it and
;; find combinations of length k from the n-1 other elements.

(defmethod  find-subgroups ((mg mgroup) (k integer))
  "Find all subgroups of order k by checking all combinations of lenth k
from the set of elements. Return a list of them."
  (let ((result nil))
    (alexandria:map-combinations
     #'(lambda (set)
	 (when (is-group-p set (binary-op mg))
	   (push (make-subgroup mg set) result)))
     (elements mg)
     :length k)
    (reverse result)))

;; Convenience functions.

(defun z/nz*-show (n &key (width 5))
  (let ((foo (make-z/nz*-group n)))
    (print-cayley-table foo :width width)
    foo))



;; Tests ==================================================================

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
  (5am:is (equal '(1 2 3 4 5 6 7 8) (z/nz-residues 9)))
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

(5am:def-test is-it-a-group? ()
  (let ((op (mod-binary-op-* 9)))
    (5am:is (has-1-p '(1 2 3 4)))
    (5am:is (not (has-1-p '(2 3 4))))
    (5am:is (has-closure-p '(1 2 4 5 7 8) op))
    (5am:is (not (has-closure-p '(1 2 4 5 7) op)))
    (5am:is (has-right-inverses-p '(1 2 4 5 7 8) op))
    (5am:is (not (has-right-inverses-p '(1 2 4 5 6 7 8) op)))))


(defun do-mgroups-tests ()
  (5am:run! 'mgroups-test-suite))
