(in-package "LISP")


(defun complex (real img) (make-instance complex :real real :imaginary img))

(set-dispatch-macro-character #\# #\C
 #'(lambda (s n c)
     (multiple-value-bind (r i) (read s)
       (complex r i))))

(export '(complex))
