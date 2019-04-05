(eval-when (load eval)

  (in-package "LISP")
  (defun assert (pred &optional (message "assertation error") &rest args)
    (if (not pred)
        (apply #'error message args)))
)
