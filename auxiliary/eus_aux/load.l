(in-package "LISP")
(export '(*loader-current-file*))

(defvar *loader-current-file* nil)
(defvar +load-fn+ (symbol-function 'load))
(defun load (fname &rest load-args)
  (let ((*loader-current-file*
         (namestring (make-pathname :name (pathname-name fname)
                                    :type (pathname-type fname)))))
    (apply +load-fn+ fname load-args))))
