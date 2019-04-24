;; Provisory file
;; Pull request in https://github.com/euslisp/EusLisp/pull/363

(in-package "LISP")

(defun assert (pred &optional (message "assertation error") &rest args)
  (if (not pred)
      (apply #'error message args)))
