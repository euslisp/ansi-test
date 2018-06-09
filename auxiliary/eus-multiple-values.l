(defvar magic (cons 'multiple 'values))


(defun magicp (x)
  (and (consp x) (eq (car x) magic)))

(defun list-values (&rest args)
  "Takes a multiple-arguments return of values and make it a - simple - list"
  (if (magicp (car args))
      (cdr (car args))
      (car args)))

(defun values (&rest args)
  (if (and (not (null args))(null (cdr args)))
      (car args)
      (cons magic args)))

(defun values-list (list)
  "All of the values of list are returned as multiple values"
  (apply #'values list))

(defun call-with-values (producer consumer)
  "Producer is a no-arguments function whose return values are fed to consumer"
  (let ((x (funcall producer)))
    (if (magicp x)
        (apply consumer (cdr x))
        (funcall consumer x))))


(defmacro multiple-value-call (fun &rest forms)
  "Reunites the return values for each form in form and applies fun to them"
    `(call-with-values #'(lambda()
                           (let ((form-values (list ,@forms)))
                             (values-list
                              (loop for el in form-values append (list-values
                                                                  (if (consp el)
                                                                      el
                                                                      (list el)))))))
                               ,fun))


;;
;; Common Lisp standard multiple-value macros (based on multiple-value-call)
;;


(defmacro multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))


;; (defmacro multiple-value-bind (varlist value-form &rest body)
;;       (let ((ignore (gensym)))
;; 	`(multiple-value-call #'(lambda (&optional ,@(mapcar #'list varlist) &rest ,ignore)
;; 				  (declare (ignore ,ignore))
;; 				  ,@body)
;; 	   ,value-form))))

(defmacro multiple-value-bind (varlist value-form &rest body)
  `(multiple-value-call #'(lambda (,@varlist)
                               ,@body)
                           ,value-form))

;; (defmacro multiple-value-setq (varlist value-form)
;;   (if varlist
;;       `(values (setf (values ,@varlist) ,value-form))
;;       `(values ,value-form)))

(defmacro multiple-value-prog1 (form &rest forms)
  `(progn ,@forms ,form))
     
