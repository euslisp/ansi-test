(in-package "LISP")

(defmacro lambda (args &rest body) `(function (lambda ,args ,@body)))

(defmacro ignore (&rest syms)
  `(progn
     ,@(mapcar #'(lambda (sym) `(defmacro ,sym (&rest args))) syms)
     (export ',syms)))

(ignore declare declaim handler-bind ignore-errors)
