;;; argparse.l

;; -------------------
;; Simple Command Line Argument Parser for EusLisp
;; Author: Guilherme de Campos Affonso
;; Created at 2019
;; -------------------


;; Example usage

#|
(require :argparse "argparse.l")

(defvar argparse (instance argparse:argument-parser :init "Program Description (optional)"))
(send argparse :add-argument '("--foo" "-f") :help "the foo description")
(send argparse :add-argument "--bar" :name 'bar-argument
      :callback #'(lambda () (print "Bar callback!")))

(send argparse :parse-args)
(format t "foo: ~A~%" (send argparse :foo))
(format t "bar: ~A~%" (send argparse :bar-argument))
(exit)
|#

;; Code

(unless (find-package "ARGPARSE")
  (make-package "ARGPARSE"))
(in-package "ARGPARSE")

(export '(argument-parser))

;; Utility
(defun mklst (obj) (if (listp obj) obj (list obj)))
(defun no-args-p (fn)
  "Return t if the function has null argument list. Unable to handle compiled functions"
  (if (symbolp fn) (setq fn (symbol-function fn)))
  (and (listp fn) (not (nth 4 fn))))

(defun find-any (item-lst seq &rest find-args)
  (dolist (item item-lst)
    (if (apply #'find item seq find-args) (return-from find-any t))))

(defmacro set-synonyms (&rest names)
  `(progn
     ,@(mapcar #'(lambda (place) `(send self :set-val ',place ,place)) names)))
(defun cli-error (format-string &rest args)
  (eval
   `(let ((lisp::*max-callstack-depth* 0))
      (let ((*error-output* *standard-output*))
        (warning-message 1 ,(apply #'format nil format-string args))
        (terpri *error-output*))
      (error ,(apply #'format nil format-string args)))))

(defun flag-name-p (str)
  (and (stringp str) (not (string= (string-left-trim "-" str) "")) (= (elt str 0) #\-)))
(defun check-flag (str)
  (and (flag-name-p str) (not (find #\= str))))
(defun split-flag (str)
  "Returns name, value, setp"
  (let ((pos (position #\= str)))
    (values
     (if pos (subseq str 0 pos) str)
     (if pos (subseq str (1+ pos)))
     (not (not pos)))))
(defun make-flag-dest (obj)
  (if (and (symbolp obj) (not (symbol-package obj)))
      obj
      (intern (string-left-trim "-" (string-upcase obj)) *keyword-package*)))

;; Class Definition
(defclass argparse-docstring
  :slots (usage-stream details-stream description epilog))
(defmethod argparse-docstring
  (:init (&key prog description epilog)
    (set-synonyms description epilog)
    (setq usage-stream (make-string-output-stream))
    (setq details-stream (make-string-output-stream))
    ;; print initial message
    (princ "usage:" usage-stream)
    (if prog (format usage-stream " ~A" prog))
    (format details-stream "optional arguments:~%")
    self)
  (:add-argument (flags help default required &optional name)
    (setq flags (sort flags #'< #'length))
    (if name (setq name (string-upcase name)))
    (let ((name-str (format nil "~A~A" (car flags) (if name (format nil "=~A" name) ""))))
      (if required
          (format usage-stream " ~A" name-str)
          (format usage-stream " [~A]" name-str)))
    (princ "  " details-stream)
    (maplist #'(lambda (a)
                 (format details-stream "~A~A" (car a)
                         (if name (format nil "=~A" name) ""))
                 (if (cdr a) (format details-stream ", ")))
             flags)
    (if help (format details-stream "~T~A" help))
    (if default (format details-stream "~C(default: ~S)" (if help #\Space #\Tab) default))
    (terpri details-stream))
  (:print-help (&optional (stream *standard-output*))
    (format stream "~A~%~%" (get-output-stream-string usage-stream))
    (if description (format stream "~A~%~%" description))
    (format stream "~A~%" (get-output-stream-string details-stream))
    (if epilog (format stream "~A~%" epilog))))

(defclass argparse-argument
    :slots (value action const count flagp read required))
(defmethod argparse-argument
  (:init (&key action const default read required)
    (set-synonyms action const read required)
    (setq value (case action
                  (:store-true nil)
                  (:store-false t)
                  (t default)))
    (setq count 0)
    (setq flagp (if (functionp action)
                    (no-args-p action)
                    (not (not (memq action '(:store-true :store-false :store-const :count))))))
    self)
  (:value () value)
  (:store (val) (setq value (if read (read-from-string val) val)))
  (:store-true () (send self :store t))
  (:store-false () (send self :store nil))
  (:store-const () (send self :store const))
  (:count () (send self :store (incf count)))
  (:collect (val) (setq value (append value (list (if read (read-from-string val) val))))))

(defclass argument-parser
  :super propertied-object
  :slots (flaglst docstring))
(defmethod argument-parser
  (:init (&key prog description epilog (add-help t))
    (setq docstring (instance argparse-docstring :init
                              :prog prog
                              :description description
                              :epilog epilog))
    (if add-help
        (send self :add-argument '("--help" "-h") :help "show this help message and exit"
              :dest (gensym)
              :action `(lambda () (send ,docstring :print-help) (exit))))
    self)
  (:add-argument (flags &key (action :store) const default read required help dest)
    (let* ((flags (mklst flags))
           (name (make-flag-dest (or dest (car flags))))
           (arg (instance argparse-argument :init
                          :default default
                          :read read
                          :action action
                          :required required
                          :const const)))
      ;; check type
      (dolist (a flags)
        (unless (check-flag a)
          (cli-error "Invalid argument name in ~S" a)))
      ;; ensure name is unique
      (if (assoc name (send self :plist))
          (cli-error "Already have argument with name ~S" name)
          (setf (get self name) arg))
      ;; add to flaglst
      (dolist (a flags)
        (if (assoc a flaglst :test #'string=)
            (cli-error "Multiple flags for ~S" a))
        (push (cons a name) flaglst))
      ;; add to docstring
      (send docstring :add-argument flags help (send arg :value) required
            (unless (argparse-argument-flagp arg) name))
      ;; add method
      (if (keywordp name)
          (eval `(defmethod argument-parser (,name () (send (get self ,name) :value)))))
      ;; return name
      name))
  (:parse-args ()
    (let ((required-args (remove-if-not #'(lambda (obj) (argparse-argument-required (cdr obj)))
                                        plist)))
      (dolist (flag (remove-if-not #'flag-name-p lisp::*eustop-argument*))
        (multiple-value-bind (name value setp) (split-flag flag)
          (let* ((arg-name (cdr (assoc name flaglst :test #'string=)))
                 (arg (get self arg-name)))
            (if arg
                ;; has flag
                (let ((action (argparse-argument-action arg)))
                  (if required-args
                      (setq required-args (delete arg required-args :count 1 :key #'cdr)))
                  (cond
                    ((functionp action)
                     (if setp
                         (send arg :store (funcall action value))
                         (send arg :store (funcall action))))
                    ((memq action (send arg :methods))
                     (if setp
                         (send arg action value)
                         (send arg action)))
                    (t (cli-error "Action ~A not found!" action))))
                ;; doesn't have flag
                (warning-message 1 "Argument ~S not found!~%" flag)))))
      ;; check required arguments
      (when required-args
        (cli-error "Argument ~A is required!" (caar (last required-args)))))))
