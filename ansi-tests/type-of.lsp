;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jun  4 21:15:05 2003
;;;; Contains: Tests of TYPE-OF

(in-package :cl-test)

;;;  It turns out I left out an important test of type-of:
;;;  (type-of x) must be a recognizable subtype of every builtin type
;;;  of which x is a member.

(deftest type-of.1
  (loop for x in *universe*
	for tp = (type-of x)
	for failures = (loop for tp2 in *cl-all-type-symbols*
			     when (and (typep x tp2)
				       (not (subtypep tp tp2)))
			     collect tp2)
	when failures collect (list x failures))
  nil)

;;; 1. For any object that is an element of some built-in type:
;;;  b. the type returned does not involve and, eql, member, not,
;;;     or, satisfies, or values.
;;;
;;; Since every object is an element of the built-in type T, this
;;; applies universally.

(deftest type-of.2
  (loop for x in *universe*
	for tp = (type-of x)
	when (and (consp tp)
		  (member (car tp) '(and eql member not or satisfies values
					 function)))
	collect x)
  nil)

(deftest type-of.3
  (loop for x in *universe*
	unless (typep x (type-of x))
	collect x)
  nil)

(deftest type-of.4
  (loop for x in *universe*
	for tp = (type-of x)
	for class = (class-of x)
	unless (equal (multiple-value-list (subtypep* tp class)) '(t t))
	collect x)
  nil)

(deftest type-of.5
  (loop for x in *cl-condition-type-symbols*
	for cnd = (make-condition x)
	for tp = (type-of cnd)
	unless (eq x tp)
	collect x)
  nil)

(defstruct type-of.example-struct a b c)

(deftest type-of.6
  (type-of (make-type-of.example-struct))
  type-of.example-struct)

(defclass type-of.example-class () ())

(deftest type-of.7
  (type-of (make-instance 'type-of.example-class))
  type-of.example-class)

(defparameter *type-of.example-class-2*
  (defclass type-of.example-class-2 () ((a) (b) (c))))

(deftest type-of.8
  (progn
    (setf (class-name *type-of.example-class-2*) nil)
    (eqt (type-of (make-instance *type-of.example-class-2*))
	 *type-of.example-class-2*))
  t)

;;; Error tests

(deftest type-of.error.1
  (classify-error (type-of))
  program-error)

(deftest type-of.error.2
  (classify-error (type-of nil nil))
  program-error)  