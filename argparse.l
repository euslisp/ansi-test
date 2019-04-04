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

(defclass argparse-docstring
  :super propertied-object
  :slots (usage-stream details-stream))
(defmethod argparse-docstring
  (:init (&optional description)
    (setq usage-stream (make-string-output-stream))
    (setq details-stream (make-string-output-stream))
    ;; print initial message
    (princ "usage:" usage-stream)
    (if description
      (format details-stream "~A~%~%" description))
    (format details-stream "optional arguments:~%")
    self)
  (:add-argument (args help)
    (setq args (sort args #'< #'length))
    (format usage-stream " [~A]" (car args))
    (princ "  " details-stream)
    (maplist #'(lambda (a)
                 (format  details-stream "~A" (car a))
                 (if (cdr a) (format details-stream ", ")))
             args)
    (if help (format details-stream "~T~A" help))
    (terpri details-stream))
  (:print-help (&optional exit)
    (format t "~A~%~%~A~%"
            (get-output-stream-string usage-stream)
            (get-output-stream-string details-stream))
    (if exit (exit))))

(defclass argument-parser
  :super propertied-object
  :slots (flags callbacks docstring))
(defmethod argument-parser
  (:init (&optional description)
    (setq docstring (instance argparse-docstring :init description))
    (send self :add-argument '("--help" "-h") :help "show this help message and exit" 
          :name (gensym)
          :callback `(lambda () (send ,docstring :print-help t)))
    self)
  (:check-flag (str)
    (and (stringp str) (>= (length str) 2)
         (= (elt str 0) #\-)))
  (:add-argument (args &key help name callback)
    (flet ((mklst (obj)
             (if (listp obj) obj (list obj)))
           (mkkey (obj)
             (if (and (symbolp obj) (not (symbol-package obj)))
                 obj
                 (intern (string-left-trim "-" (string-upcase obj))
                         *keyword-package*))))
      (let* ((args (mklst args))
             (name (mkkey (or name (car args)))))
        ;; check `args' type
        (unless (every #'stringp args)
          (error "string expected"))
        ;; ensure name is unique
        (if (assoc name (send self :plist))
            (error (format nil "Already have argument with name ~S" name))
            (setf (get self name) nil))
        ;; add to flags
        (dolist (a args)
          (unless (send self :check-flag a)
            (error "Arguments must start with '-'"))
          (if (assoc a flags :test #'string=)
              (warning-message 1 "Multiple flags for ~S~%" a))
          (push (cons a name) flags))
        ;; add to callbacks
        (if callback
            (push (cons name callback) callbacks))
        ;; add to docstring
        (send docstring :add-argument args help)
        ;; add method
        (if (keywordp name)
            (eval `(defmethod argument-parser (,name () (get self ,name)))))
        ;; return name
        name)))
  (:parse-args ()
    (dolist (arg (remove-if-not #'(lambda (a) (send self :check-flag a))
                                lisp::*eustop-argument*))
      (let ((name (cdr (assoc arg flags :test #'string=))))
        (if name
          ;; has flag
          (let ((cb (cdr (assoc name callbacks))))
            (if cb (funcall cb))
            (setf (get self name) t))
          ;; doesn't have flag
          (warning-message 1 "Argument not found: ~S~%" arg))))))
