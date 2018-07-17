(in-package "LISP")

(defun stringify-name (name)
  (cond
      ((stringp name) name)
      ((symbolp name) (symbol-name name))
      (t (string name))))

(defun stringify-names (names)
  (mapcar #'(lambda (name)
              (stringify-name name))
          names))

;;;; DEFPACKAGE:

(defvar *default-package-use-list* :user)

(defmacro defpackage (package &rest options)
  "Defines a new package called PACKAGE.  Each of OPTIONS should be one of the
   following:
     (:NICKNAMES {package-name}*)
     (:SIZE <integer>)
     (:SHADOW {symbol-name}*)
     (:SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
     (:USE {package-name}*)
     (:IMPORT-FROM <package-name> {symbol-name}*)
     (:INTERN {symbol-name}*)
     (:EXPORT {symbol-name}*)
     (:DOCUMENTATION doc-string)
   All options except :SIZE and :DOCUMENTATION can be used multiple times."
  (let ((nicknames nil)
	(size nil)
	(shadows nil)
	(shadowing-imports nil)
	(use nil)
	(use-p nil)
	(imports nil)
	(interns nil)
	(exports nil)
	(doc nil))
    (dolist (option options)
      (unless (consp option)
	(error "Bogus DEFPACKAGE option"))
      (case (car option)
	(:nicknames
	 (setf nicknames (stringify-names (cdr option))))

	(:size
	 (cond
          (size (error "Can't specify :SIZE twice."))
          ((and (consp (cdr option))
                (typep (second option) 'natural))
           (setf size (second option)))
          (t
           (error
            "Bogus :SIZE, must be a positive integer: ~S"
            (second option)))))

	(:shadow
	 (let ((new (stringify-names (cdr option))))
	   (setf shadows (append shadows new))))

	(:shadowing-import-from
	 ;; (let ((package-name (stringify-name (second option) "package"))
	 ;;       (names (stringify-names (cddr option) "symbol")))
	 ;;   (let ((assoc (assoc package-name shadowing-imports
	 ;;        	       :test #'string=)))
	 ;;     (if assoc
	 ;;         (setf (cdr assoc) (append (cdr assoc) names))
	 ;;         (setf shadowing-imports
	 ;;               (acons package-name names shadowing-imports)))))
         (error "shadowing-import-from not supported yet(?)"))

	(:use
	 (let ((new (stringify-names (cdr option))))
	   (setf use (delete-duplicates (nconc use new) :test #'string=))
	   (setf use-p t)))

	(:import-from
	 (let ((package-name (stringify-name (second option)))
	       (names (stringify-names (cddr option))))
	   (let ((assoc (assoc package-name imports
			       :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
		 (setf imports (acons package-name names imports))))))

	(:intern
	 (let ((new (stringify-names (cdr option))))
	   (setf interns (append interns new))))

	(:export
	 (let ((new (stringify-names (cdr option))))
	   (setf exports (append exports new))))

	(:documentation
	 (when doc
	   (error "Can't specify :DOCUMENTATION twice."))
	 (setf doc (coerce (second option) string)))
	(t
	 (error "Bogus DEFPACKAGE option: ~S" option))))

    (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
    (check-disjoint `(:intern ,@interns)
        	    `(:import-from
        	      ,@(apply #'append (mapcar #'rest imports)))
        	    `(:shadow ,@shadows)
        	    `(:shadowing-import-from
        	      ,@(apply #'append (mapcar #'rest shadowing-imports))))

    `(eval-when (compile load eval)
       (OOdefpackage ,(stringify-name package) ',nicknames ',size
		    ',shadows ',shadowing-imports ',(if use-p use :default)
		    ',imports ',interns ',exports ',doc))))

(defun mintersection (sets)
  (cond
   ((null sets) '())
   ((null (third sets))
    (intersection (first sets) (second sets)))
   ((intersection (first sets)
               (mintersection (cdr sets))))))

(defmacro check-disjoint (&rest args)
  ;; Check whether given arguments specify disjoint sets of symbols
  ;; Each argument is of the form (:key . set)

  (when (mintersection (mapcar #'cdr args))
    (error "Parameters must be disjoint, but have common elements")))

(defun OOdefpackage (name nicknames size shadows shadowing-imports
			 use imports interns exports doc-string)
  ;; (declare (type simple-base-string name)
  ;;          (type list nicknames shadows shadowing-imports
  ;;       	 imports interns exports)
  ;;          (type (or list (member :default)) use)
  ;;          (type (or simple-base-string null) doc-string))
  (let ((package (or (find-package name)
		     (progn
		       (when (eq use :default)
		     	     (setf use *default-package-use-list*))
		       (make-package name :use nil)))))
    (unless (string= (package-name package) name)
      (error "The passed name is a nick-name for the package"))

    ;; Nicknames
    (send package :nicknames nicknames)

    ;; Shadows and Shadowing-imports.
    ;; (let ((old-shadows (package-%shadowing-symbols package)))
    ;;   -> need a way of fetching shadowed symbols
    (when shadows
      (dolist (shado shadows) (shadow shado package)))
    ;; (dolist (sym-name shadows)
    ;; (setf old-shadows (remove (find-symbol sym-name package) old-shadows)))
    ;; (dolist (simports-from shadowing-imports)
    ;;   (let ((other-package (package-or-lose (car simports-from))))
    ;;     (dolist (sym-name (cdr simports-from))
    ;;       (let ((sym (find-or-make-symbol sym-name other-package)))
    ;;         (shadowing-import sym package)
    ;;         (setf old-shadows (remove sym old-shadows))))))
    ;;  -> need a way of shadowing-import
    ;; (when old-shadows
    ;;   (warn (intl:gettext "~A also shadows the following symbols:~%  ~S")
    ;;         name old-shadows)))

    ;; Use
    (unless (eq use :default)
      (let ((old-use-list (package-use-list package))
	    (new-use-list (mapcar #'find-package use)))
	(use-package (set-difference new-use-list old-use-list) package)
	(let ((laterize (set-difference old-use-list new-use-list)))
	  (when laterize
	    (unuse-package laterize package)
	    (warn "~A previously used the following packages:~%  ~S"
		  name
		  laterize)))))

    ;; Import and Intern
    (dolist (sym-name interns)
      (intern sym-name package))
    (dolist (imports-from imports)
      (let ((other-package (find-package (car imports-from))))
	(dolist (sym-name (cdr imports-from))
          (if (find-symbol sym-name other-package)
	  (import (list (find-symbol sym-name other-package))
		  package)))))
    ;; Exports.
    (let ((old-exports nil)
	  (exports (mapcar #'(lambda (sym-name) (intern sym-name package))
			   exports)))
      (do-external-symbols (sym package)
	(push sym old-exports))
      (export exports package)
      (let ((diff (set-difference old-exports exports)))
	(when diff
	  (warn "~A also exports the following symbols:~%  ~S" name diff))))
    ;; Documentation
    ;;(setf (lisp::package-doc-string package) doc-string)
    package))

(export '(defpackage))
