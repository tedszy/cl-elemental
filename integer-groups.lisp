(in-package :integer-groups)

;; This package is for doing simple calculations with groups
;;
;; Z/nZ and (Z/nZ)*
;;
;; Z/nZ is the additive integer group mod n. For example,
;; when n = 10 we have the elements
;;
;; 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
;;
;; with binary operation + mod 10 and identity 1. This forms a group.
;;
;; However it's not a group under * mod 10. That's because
;; some elements do not have inverses. To make a group out
;; of them, eliminate the ones not coprime to n. Note that
;; gcd(0,n) = n so 0 is eliminated. We are left with
;;
;; 1, 3, 7, 9.
;;
;; With binary operation * mod 10 and identity 1, this forms
;; a group called (Z/10Z)*.
;;
;; This package provides classes ZGROUP, AGROUP and MGROUP.
;; ZGROUP is the base class for integer groups so it's not
;; intended to be used directly.
;;
;; Instances of AGROUP and MGROP are the Z/nZ and (Z/nZ)*.
;; Create them using make-agroup and make-mgroup.
;;
;; Examine cayley tables by calling print-cayley-table on
;; your group instances.
;;
;; You can find subgroups of order k with find-subgroups.
;; This is a primitive brute-force method that examines
;; all combinations of length k from the set of elements
;; of the group. Nevertheless it works fine for (very) small
;; groups.

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
  (:documentation "Base class forinteger groups."))

(defclass agroup (zgroup)
  ((modulus :initarg :modulus :accessor modulus))
  (:documentation "Class representing Z/nZ additive residue groups mod n."))

(defclass mgroup (zgroup)
  ((modulus :initarg :modulus :accessor modulus))
  (:documentation "Class representing (Z/nZ)* multiplicative groups mod n."))

(defun make-zgroup (elements binary-op id &key (binary-op-label "@"))
  "Constructor for base class: used in creating subgroups."
  (make-instance 'zgroup :binary-op binary-op
			 :elements elements
			 :id id
			 :cayley-table (make-cayley-table elements binary-op)
			 :binary-op-label binary-op-label
			 :order (length elements)))

(defun make-agroup (n)
  "Constructor for Z/nZ groups."
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
  "Constructor for (Z/nZ)* groups."
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

(defun print-cayley-table (group &key (width 3))
  "Use this to display a group's Cayley table."
  (with-slots (binary-op cayley-table)
      group
    (display-table cayley-table :width width)))

(defun make-subgroup (group elements)
  "Constructor for subgroups. A subgroup is a more general object 
so it is intantiated as a ZGROUP."
  (with-slots (binary-op id binary-op-label)
      group
    (make-zgroup elements binary-op id :binary-op-label binary-op-label)))

(defun has-identity? (id elements)
  "Is id among the elements?"
  (member id elements))

(defun is-closed? (op elements)
  "Every op-product of two elements must be in the element set."
  (loop for e in elements
	always (loop for f in elements
		     always (member (funcall op e f) elements))))

(defun has-right-inverses? (op id elements)
  "For every e in elements, there must be a unique f such that e*f=id."
  (loop for e in elements
	always (= 1 (loop for f in elements counting (= id (funcall op e f))))))

(defun is-subgroup? (op id elements)
  "System is a subgroup if it has identity, closure, and inverses."
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
  "Create (Z/nZ)*, print its Cayley table, and return the instance."
  (let ((foo (make-mgroup n)))
    (print-cayley-table foo :width width)
    foo))

(defun ag-show (n &key (width 5))
  "Create Z/nZ, print its Cayley table, and return the instance."
  (let ((foo (make-agroup n)))
    (print-cayley-table foo :width width)
    foo))

(defgeneric left-coset (group element subset)
  (:documentation "Pick an element a from group G. 
Let H be a subset of G. Then the left coset is the set a*h for all h in H."))

(defmethod left-coset ((group zgroup) (a integer) (subset list))
  "Subset H, element a, left coset {a*h for all h in H}.
a must be in G."
  (assert (member a (elements group))
	  (a)
	  "element ~a is not in group ~a" a group)
  (assert (subsetp subset (elements group))
	  (subset)
	  "set ~a is not a subset of ~a" subset group)
  (sort (loop for h in subset collect (funcall (binary-op group) a h)) #'<))

(defmethod left-coset ((group zgroup) (a integer) (subgroup zgroup))
  "Calculate coset of subgroup H, but this time H really is a subgroup 
and not just a subset."
  (assert (member a (elements group))
	  (a)
	  "element ~a is not in group ~a" a group)
  (assert (subsetp (elements subgroup) (elements group))
	  (subgroup)
	  "set ~a is not a subset of ~a" (elements subgroup) group)
  (sort (loop for h in (elements subgroup) collect (funcall (binary-op group) a h)) #'<))

(defun group-op (group a b)
  "Apply the group binary operation to a, b."
  (funcall (binary-op group) a b))

(defun group-inverse (group a)
  "Return the group inverse of element a."
  (assert (member a (elements group))
	  (a)
	  "element ~a is not in group ~a" a group)
  (loop for x in (elements group)
	when (= (id group) (group-op group x a))
	  return x))

(defgeneric left-coset-partition (group subset)
  (:documentation "Partition the group G elements into disjoint unique
left-cosets of a subgroup. The subgroup can be specified as a list of
elements (subset of G) or a ZG subgroup instance."))

(defmethod left-coset-partition ((group zgroup) (subgroup list))
  "Collect all the unique cosets of subgroup H. This collection 
partitions the group G into equal-sized disjoint subsets."
  (assert (subsetp subgroup (elements group))
	  (subgroup)
	  "set ~a is not a subset of ~a" subgroup group)
  (assert (is-subgroup? (binary-op group) (id group) subgroup)
	  (subgroup)
	  "set ~a is not a subgroup of ~a" subgroup group)
  (let ((result nil))
    (loop for a in (elements group)
	  do (pushnew
	      (sort
	       (loop for h in subgroup
		     collecting (group-op group a h))
	       #'<)		    
	      result
	      :test #'equal))
    result))

(defmethod left-coset-partition ((group zgroup) (subgroup zgroup))
  "Collect all the unique cosets of subgroup H. This collection 
partitions the group G into equal-sized disjoint subsets."
  (assert (subsetp (elements subgroup) (elements group))
	  (subgroup)
	  "group ~a is not a subset of ~a" subgroup group)
  (assert (is-subgroup? (binary-op group) (id group) (elements subgroup))
	  (subgroup)
	  "group ~a is not a subgroup of ~a" subgroup group)
  (let ((result nil))
    (loop for a in (elements group)
	  do (pushnew
	      (sort
	       (loop for h in (elements subgroup)
		     collecting (group-op group a h))
	       #'<)		    
	      result
	      :test #'equal))
    result))

(defun group-expt (group a n)
  "a in group G, compute a^n."
  (let ((result (id group)))
    (do ((k 1 (1+ k)))
	((> k n) result)
      (setf result (group-op group a result)))))

(defun order-of-element (group a)
  "Smallest exponent n such that a^n = id."
  (do ((k 1 (1+ k))
       (result a (group-op group a result)))
      ((= result (id group)) k)))



;; Table of inverses.
;; Table of element orders.



;; Tests ==================================================================

(5am:def-suite integer-groups-test-suite
  :description "Check how well INTEGER-GROUPS work.")

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

(5am:def-test coset-property-1 ()
  "When H < G, a in G, a is always in aH."
  (let* ((group-G (make-mgroup 55))
	 (subgroup-H (make-subgroup group-G '(1 12 21 23 32 34 43 54))))
    (5am:is (loop for a in (elements group-G)
		  always (member a (left-coset group-G a subgroup-H))))))

(5am:def-test coset-property-2 ()
  "H < G, a in G, then a in H if and only if aH = H"
  ;; Test a in H ==> aH=H
  (let* ((group-G (make-mgroup 55))
	 (a 23)
	 (subgroup-H (make-subgroup group-G '(1 12 21 23 32 34 43 54)))
	 (coset-aH (left-coset group-G a subgroup-H)))
    (5am:is (equal coset-aH (elements subgroup-H)))))

(5am:def-test coset-property-3 ()
  "(ab)H = a(bH)"
  (let* ((group-G (make-mgroup 65))
	 (a 14)
	 (b 41)
	 (subgroup-H (make-subgroup group-G '(1 14 27 53))))
    (5am:is (equal (left-coset group-G (group-op group-G a b) subgroup-H)
		   (left-coset group-G a (left-coset group-G b subgroup-H))))))

(5am:def-test coset-property-4 ()
  "aH=bH if and only if a in bH or b in aH."
  (let* ((group-G (make-mgroup 65))
	 (subgroup-H (make-subgroup group-G '(1 9 14 16 29 61)))
	 (a 17)
	 (b 38)
	 (coset-aH (left-coset group-G a subgroup-H))
	 (coset-bH (left-coset group-G b subgroup-H)))
    (5am:is (and (equal coset-aH coset-bH)
		 (member a coset-bH)
		 (member b coset-aH)))))

(5am:def-test coset-property-5 ()
  "Either aH=bH or aH intersect bH = empty."
  (let* ((group-G (make-mgroup 65))
	 (a1 17)
	 (b1 38)
	 (a2 11)
	 (b2 54)
	 (subgroup-H (make-subgroup group-G '(1 9 14 16 29 61))))
    (5am:is (equal (left-coset group-G a1 subgroup-H)
		   (left-coset group-G b1 subgroup-H)))
    (5am:is (equal nil (intersection
			(left-coset group-G a2 subgroup-H)
			(left-coset group-G b2 subgroup-H))))))

(5am:def-test coset-property-6 ()
  "aH=bH if and only if inv(a)b is in H."
  ;; Test inv(a)b in H ==> aH=bH.
  (let* ((group-G (make-mgroup 58))
	 (subgroup-H (make-subgroup group-G '(1 7 23 25 45 49 53)))
	 ;; Note that neither a1 nor a2 are in subgroup-H
	 (a1 11)
	 (b1 31))
    (flet ((foo (a b) (group-op g58 (group-inverse g58 a) b)))
      (5am:is (member (foo a1 b1) (elements subgroup-H)))
      (5am:is (equal (left-coset group-G a1 subgroup-H)
		     (left-coset group-G b1 subgroup-H))))))

(5am:def-test coset-property-7 ()
  "|aH| = |bH|."
  (let* ((group-G (make-mgroup 58))
	 (subgroup-H (make-subgroup group-G '(1 7 23 25 45 49 53)) )
	 (a1 19)
	 (b1 49)
	 (a2 3)
	 (b2 39))
    (5am:is (alexandria:length=
	     (left-coset group-G a1 subgroup-H)
	     (left-coset group-G b1 subgroup-H)))
    (5am:is (alexandria:length=
	     (left-coset group-G a2 subgroup-H)
	     (left-coset group-G b2 subgroup-H)))))

;; Coset property 8: aH=Ha iff H = aH*inv(a) is always true
;; for integer groups because integer groups are abelian.

(5am:def-test coset-property-9 ()
  "H<G, a in G. aH<G if and only if a in H."
  (let* ((group-G (make-mgroup 58))
	 (subgroup-H (make-subgroup group-G '(1 7 23 25 45 49 53)))
	 (a1 45)
	 (a2 13))
    (5am:is (is-subgroup? (binary-op group-G) 1 (left-coset g58 a1 subgroup-H)))
    (5am:is (not (is-subgroup? (binary-op group-G) 1 (left-coset g58 a2 subgroup-H))))))

(5am:def-test group-exponents/group-order ()
  "When a in G, then a^|G| = id."
  (let* ((group-G (make-mgroup 75))
	 (group-G2 (make-agroup 19)))
    (5am:is (loop for a in (elements group-G)
		  always (= (id group-G) (group-expt group-G a (order group-G)))))
    (5am:is (loop for a in (elements group-G2)
		  always (= (id group-G2) (group-expt group-G2 a (order group-G2)))))))

(5am:def-test order-of-elements ()
  "order of a in G is smallest n such that a^n = id."
  (let ((mg15 (make-mgroup 15))
	(ag10 (make-agroup 10)))
    (5am:is (= 4 (order-of-element mg15 7)))
    (5am:is (= 2 (order-of-element mg15 11)))
    (5am:is (= 5 (order-of-element ag10 2)))
    (5am:is (= 1 (order-of-element ag10 0)))))


(defun do-integer-groups-tests ()
  "Runs the FiveAM tests for integer-groups."
  (5am:run! 'integer-groups-test-suite)
  'done)
