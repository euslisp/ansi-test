;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 07:34:40 1998
;;;; Contains: Tests of ENDP

(in-package :cl-test)

(compile-and-load "cons-aux.lsp")

(deftest endp-nil
  (notnot-mv (endp nil))
  t)

(deftest endp-cons
  (endp (cons 'a 'a))
  nil)

(deftest endp-singleton-list
  (endp '(a))
  nil)

(deftest endp.order.1
  (let ((i 0))
    (values
     (endp (progn (incf i) '(a b c)))
     i))
  nil 1)

(deftest endp-symbol-error
  (catch-type-error (endp 'a))
  type-error)

(deftest endp-fixnum-error
  (catch-type-error (endp 1))
  type-error)

(deftest endp-float-error
  (catch-type-error (endp 0.9212d4))
  type-error)

(deftest endp.error.4
  (signals-error (endp) program-error)
  t)

(deftest endp.error.5
  (signals-error (endp nil nil) program-error)
  t)

(deftest endp.error.6
  (signals-error (locally (endp 1)) type-error)
  t)

