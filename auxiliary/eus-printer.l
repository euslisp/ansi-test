;;;
;;; Functions, macros and macros aimed for improving EusLisp compliance
;;; to Common Lisp
;;;

;;; Printer - heavily inspired from CMUCL

(in-package :lisp)

(defvar std-lisp-readtable ()
  "Standard lisp readtable. This is for recovery from broken
   read-tables, and should not normally be user-visible.")

(defmacro with-standard-io-syntax (&rest body)
  "Bind the reader and printer control variables to values that enable READ
   to reliably read the results of PRINT.  These values are:
       *PACKAGE*			The COMMON-LISP-USER package
       *PRINT-ARRAY*			T
       *PRINT-BASE*			10
       *PRINT-CASE*			:UPCASE
       *PRINT-CIRCLE*			NIL
       *PRINT-ESCAPE*			T
       *PRINT-GENSYM*			T
       *PRINT-LENGTH*			NIL
       *PRINT-LEVEL*			NIL
       *PRINT-LINES*			NIL
       *PRINT-MISER-WIDTH*		NIL
       *PRINT-PPRINT-DISPATCH*          The standard pprint dispatch table
       *PRINT-PRETTY*			NIL
       *PRINT-RADIX*			NIL
       *PRINT-READABLY*			T
       *PRINT-RIGHT-MARGIN*		NIL
       *READ-BASE*			10
       *READ-DEFAULT-FLOAT-FORMAT* 	SINGLE-FLOAT
       *READ-EVAL*			T
       *READ-SUPPRESS*			NIL
       *READTABLE*			the standard readtable."
  `(Owith-standard-io-syntax #'(lambda () ,@body)))

(defun Owith-standard-io-syntax (function)
  (let ((*package* (find-package "CL-USER"))
	(*print-array* t)
	(*print-base* 10)
	(*print-case* :upcase)
	(*print-circle* nil)
	(*print-escape* t)
	(*print-gensym* t)
	(*print-length* nil)
	(*print-level* nil)
	(*print-lines* nil)
	(*print-miser-width* nil)
	;;(*print-pprint-dispatch* pp::*initial-pprint-dispatch*)
	(*print-pretty* nil)
	(*print-radix* nil)
	(*print-readably* t)
	(*print-right-margin* nil)
	(*read-base* 10)
	(*read-default-float-format* 'float)
	(*read-eval* t)
	(*read-suppress* nil)
	(*readtable* *readtable*))
        ;;(*readtable* std-lisp-readtable))
    (funcall function)))


(export '(with-standard-io-syntax))
