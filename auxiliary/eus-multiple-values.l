;;
;; Hidden vars and functions
;;

(make-package :mv-interface)
(in-package :mv-interface)

(defvar magical (cons 'multiple 'values))
(defvar is-mv-on nil)

(defun magicalp (x)
  (and (consp x) (eq (car x) mv-interface::magical)))

(defun list-values (&rest args)
  "Takes a multiple-arguments return of values and make it a - simple - list"
  (if (magicalp (car args))
      (cdr (car args))
      (car args)))

;;
;; Public functions and macros
;;

(in-package :lisp)

(defun values (&rest args)
  (if (or (and args (null (cdr args)))
          (not mv-interface::is-mv-on))
      (car args)
      (cons mv-interface::magical args)))

(defun values-list (list)
  "All of the values of list are returned as multiple values"
  (apply #'values list))

(defun call-with-values (producer consumer)
  "Producer is a no-arguments function whose return values are fed to consumer"
  (let ((x (funcall producer)))
    (if (mv-interface::magicalp x)
        (apply consumer (cdr x))
        (funcall consumer x))))


(defmacro multiple-value-call (fun &rest forms)
  "Reunites the return values for each form in form and applies fun to them"
  `(call-with-values #'(lambda()
                         (setq mv-interface::is-mv-on t)
                         (let ((form-values (list ,@forms)))
                           (progn
                             (setq mv-interface::is-mv-on nil)
                             (cons mv-interface::magical        ;; *magically* makes it 'values'-like
                              (labels ((f (vals)
                                         (if (null vals)
                                             '()
                                             (append
                                              (mv-interface::list-values
                                               (if (consp (car vals))
                                                   (car vals)
                                                   (list (car vals))))
                                              (f (cdr vals))))))
                                (f form-values))))))
                     ,fun))

;;
;; Common Lisp standard multiple-value macros (based on multiple-value-call)
;;

(defmacro multiple-value-list (value-form)
  "Takes value-form's multiple values return and turn them into a list"
  `(multiple-value-call #'list ,value-form))


;; (defmacro multiple-value-bind (varlist value-form &rest body)
;;       (let ((ignore (gensym)))
;; 	`(multiple-value-call #'(lambda (&optional ,@(mapcar #'list varlist) &rest ,ignore)
;; 				  (declare (ignore ,ignore))
;; 				  ,@body)
;; 	   ,value-form))))

(defmacro multiple-value-bind (varlist value-form &rest body)
  "Sets each variable in varlist to respective value returned by value-form and subsequently executes body"
  `(multiple-value-call #'(lambda (,@varlist)
                            ,@body)
     ,value-form))

;; (defmacro multiple-value-setq (varlist value-form)
;;   (if varlist
;;       `(values (setf (values ,@varlist) ,value-form))
;;       `(values ,value-form)))

(defmacro multiple-value-prog1 (&rest args)
  "Evaluates first form in args, evaluates the other forms and yields the values yielded by the first form"
  (let ((first (gensym "PROG1")))
    `(let ((,first ,(car args)))
         (progn . ,(cdr args)) ,first)))

(export '(values values-list multiple-value-call multiple-value-bind multiple-value-list))
