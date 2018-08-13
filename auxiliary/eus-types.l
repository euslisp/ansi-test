(in-package "LISP")


(defun find-class (obj &optional error-p) (class obj))

(defun sym-class (type)
  (cond
    ((eql type 'list) cons)
    ((memq type '(simple-string simple-base-string)) string)
    ((symbolp type) (symbol-value type))
    (t type)))

(alias'=concatenate 'concatenate)
(defun concatenate (type &rest args)
  (apply #'=concatenate (sym-class type) args))

(alias '=coerce 'coerce)
(defun coerce (obj type)
  (=coerce obj (sym-class type)))

(alias '=make-array 'make-array)
(defun make-array (dim &rest args &key element-type &allow-other-keys)
  (if (consp element-type) (setq element-type (car element-type)))
  (let* ((element-str (string-upcase element-type))
	 (elmt (cond ((substringp "CHAR" element-str) :character)
		     ((substringp "FLOAT" element-str) :float)
		     ((substringp "BIT" element-str) :bit)
		     ((or (substringp "FIXNUM" element-str)
			  (substringp "BYTE" element-str)) :integer)
		     (t 'vector))))
    (apply #'=make-array dim :element-type elmt args)))

(export '(find-class))
