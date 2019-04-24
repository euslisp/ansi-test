;; Provisory file
;; Pull request in https://github.com/euslisp/EusLisp/pull/373

(in-package "LISP")

(defmethod ratio
 (:init (num denom)
  (setq numerator num
        denominator denom)
  self)
