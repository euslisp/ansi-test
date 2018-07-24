(in-package "LISP")

(defun char< (ch &rest chars)
  "Returns T if its arguments are in increasing alphabetic order."
  (OOchar< ch chars))

(defmacro OOchar (ch chars)
  `(< ch ,@chars))

(defun char-equal (character &rest more-characters)
  "Returns T if all of its arguments are the same character.
   Case is ignored."
  (do ((clist more-characters (cdr clist)))
      ((atom clist) T)
    (unless (= (car clist) character)
      (return nil))))

(defun characterp (ch)
  "Returns T if ch is a character, false otherwise."
  (if (or (floatp ch) (derivedp ch object) (< ch 0))
      nil
      t))

;;; EQUALP -- public.
;;
(defun equalp (x y)
  "Just like EQUAL, but more liberal in several respects.
  Numbers may be of different types, as long as the values are identical
  after coercion.  Characters may differ in alphabetic case.  Vectors and
  arrays must have identical dimensions and EQUALP elements, but may differ
  in their type restriction."
  (cond ((eq x y) t)
	;;((characterp x) (and (characterp y) (char-equal x y))) <- characters are numbers
	((numberp x) (and (numberp y) (= x y)))
	((consp x)
	 (and (consp y)
	      (equalp (car x) (car y))
	      (equalp (cdr x) (cdr y))))
	((pathnamep x)
	 ;; (and (pathnamep y) (pathname= x y)))
         (error "pathname is not yet supported here. Sorry."))
	((hash-table-p x)
         (error "hash table is not yet supported here. Sorry."))
	 ;; (and (hash-table-p y)
	;;       (eql (hash-table-count x) (hash-table-count y))
	;;       (eql (hash-table-test x) (hash-table-test y))
	;;       (with-hash-table-iterator (next x)
	;; 	(loop
	;; 	 (multiple-value-bind (more x-key x-value)
	;; 	     (next)
	;; 	   (cond (more
	;; 		  (multiple-value-bind (y-value foundp)
	;; 		      (gethash x-key y)
	;; 		    (unless (and foundp (equalp x-value y-value))
	;; 		      (return-from equalp nil))))
	;; 		 (t
	;; 		  (return-from equalp t))))))))
	;; ((%instancep x)
	;;  (let* ((layout-x (%instance-layout x))
	;; 	(len (layout-length layout-x)))
	;;    (and (%instancep y)
	;; 	(eq layout-x (%instance-layout y))
	;; 	(structure-class-p (layout-class layout-x))
	;; 	(do ((i 1 (1+ i)))
	;; 	    ((= i len) t)
	;; 	  (declare (fixnum i))
	;; 	  (let ((x-el (%instance-ref x i))
	;; 		(y-el (%instance-ref y i)))
	;; 	    (unless (or (eq x-el y-el)
	;; 			(equalp x-el y-el))
	;; 	      (return-from equalp nil)))))))
	((vectorp x)
         (let ((length (length x)))
           (and (vectorp y)
		(= length (length y))
		(dotimes (i length t)
		  (let ((x-el (aref x i))
			(y-el (aref y i)))
		    (if (not (or (eq x-el y-el)
                                 (equalp x-el y-el)))
		      (return-from equalp nil)))))))
	((arrayp x)
	 (and (arrayp y)
	      (= (array-rank x) (array-rank y))
	      (dotimes (axis (array-rank x) t)
		(unless (= (array-dimension x axis)
			   (array-dimension y axis))
		  (return-from equalp nil)))
	      (dotimes (index (array-total-size x) t)
		(let ((x-el (row-major-aref x index))
		      (y-el (row-major-aref y index)))
		  (unless (or (eq x-el y-el)
			      (equalp x-el y-el))
		    (return-from equalp nil))))))
	(t nil)))

(export '(char-equal equalp char< characterp))
