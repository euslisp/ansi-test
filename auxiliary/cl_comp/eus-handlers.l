;;; This code is a modified version of public domain code part of the CMU
;;; Common Lisp project


;;;; HANDLER-BIND and SIGNAL.

(in-package :lisp)


(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  "(HANDLER-BIND ( {(type handler)}* )  body)
   Executes body in a dynamic context where the given handler bindings are
   in effect.  Each handler must take the condition being signalled as an
   argument.  The bindings are searched first to last in the event of a
   signalled condition."
  (unless (every #'(lambda (x) (and (listp x) (= (length x) 2))) bindings)
    (error "Ill-formed handler bindings."))
  `(let ((*handler-clusters*
	  (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
				bindings))
		*handler-clusters*)))
     (multiple-value-prog1
      (progn ,@forms)
      ;; Wait for any float exceptions
      #+x87 (float-wait))))

;;;; HANDLER-CASE and IGNORE-ERRORS.

(defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form
   { (type ([var]) body) }* )
   Executes form in a context with handlers established for the condition
   types.  A peculiar property allows type to be :no-error.  If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :no-error clause accepts more than one
   var specification."
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	     (multiple-value-call (lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((tag (gensym))
	      (var (gensym))
	      (annotated-cases (mapcar (lambda (case) (cons (gensym) case))
				       cases)))
	  `(block ,tag
	     (let ((,var nil))
	       (declare (ignorable ,var))
	       (tagbody
		  (handler-bind
		      ,(mapcar (lambda (annotated-case)
				 (list (cadr annotated-case)
				       `#'(lambda (temp)
					    ,(if (caddr annotated-case)
						 `(setq ,var temp)
						 '(declare (ignore temp)))
					    (go ,(car annotated-case)))))
			       annotated-cases)
		    (return-from ,tag
		      ,form))
		  ,@(mapcan
		     (lambda (annotated-case)
		       (list (car annotated-case)
			     (let ((body (cdddr annotated-case)))
			       `(return-from
				 ,tag
				  ,(if (caddr annotated-case)
				       `(let ((,(caaddr annotated-case) ,var))
					  ,@body)
				       `(locally ,@body))))))
		     annotated-cases))))))))

(defmacro ignore-errors (&rest forms)
  "Executes forms after establishing a handler for all error conditions that
   returns from this form nil and the condition signalled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

(export '(ignore-errors handler-case handler-bind *handler-clusters*))
