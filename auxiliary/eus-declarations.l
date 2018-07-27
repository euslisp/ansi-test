(in-package "LISP")


(defmacro ignore (&rest syms)
  `(progn
     ,@(mapcar #'(lambda (sym) `(defmacro ,sym (&rest args))) syms)
     (export ',syms)))

(ignore declare declaim handler-bind ignore-errors)
