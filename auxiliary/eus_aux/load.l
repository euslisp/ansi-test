(in-package "LISP")
(export '(*loader-current-file*))

(defvar *loader-current-file* nil)
(defvar +load-fn+ (symbol-function 'load))

(defun load (fname &rest load-args)
  (let ((*loader-current-file*
         (namestring
          (concatenate-pathnames (truename (car *loader-current-directory*))
                                 fname))))
    (apply +load-fn+ fname load-args)))
