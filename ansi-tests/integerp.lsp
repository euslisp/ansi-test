;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Sep  7 10:18:34 2003
;;;; Contains: Tests for INTEGERP

(in-package :cl-test)

(deftest integerp.error.1
  (classify-error (integerp))
  program-error)

(deftest integerp.error.2
  (classify-error (integerp 0 0))
  program-error)

(deftest integerp.error.3
  (classify-error (integerp nil nil))
  program-error)

(deftest integerp.1
  (loop for i in *integers*
	for vals = (multiple-value-list (integerp i))
	unless (and (= (length vals) 1)
		    (first vals))
	collect (cons i vals))
  nil)

(deftest integerp.2
  (loop for x in *universe*
	for vals = (multiple-value-list (integerp x))
	unless (and (= (length vals) 1)
		    (if (typep x 'integer)
			(first vals)
		      (not (first vals))))
	collect (cons x vals))
  nil)




