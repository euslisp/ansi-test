;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 18 19:39:34 2002
;;;; Contains: Tests of UNLESS

(in-package :cl-test)

(deftest unless.1
  (unless t)
  nil)

(deftest unless.2
  (unless nil)
  nil)

(deftest unless.3
  (unless 'b 'a)
  nil)

(deftest unless.4
  (unless nil 'a)
  a)

(deftest unless.5 (unless nil (values)))

(deftest unless.6
  (unless nil (values 1 2 3 4))
  1 2 3 4)

(deftest unless.7
  (unless 1 (values))
  nil)

(deftest unless.8
  (unless #() (values 1 2 3 4))
  nil)

(deftest unless.9
  (let ((x 0))
    (values
     (unless nil
       (incf x)
       'a)
     x))
  a 1)

;;; No implicit tagbody
(deftest unless.10
  (block done
    (tagbody
     (unless nil
       (go 10)
       10
       (return-from done 'bad))
     10
     (return-from done 'good)))
  good)

;;; (deftest unless.error.1
;;;  (classify-error (unless))
;;;  program-error)

(deftest unless.error.1
  (classify-error (funcall (macro-function 'unless)))
  program-error)

(deftest unless.error.2
  (classify-error (funcall (macro-function 'unless)
			   '(unless t)))
  program-error)

(deftest unless.error.3
  (classify-error (funcall (macro-function 'unless)
			   '(unless t) nil nil))
  program-error)