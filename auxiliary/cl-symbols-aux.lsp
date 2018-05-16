;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 28 06:43:51 2002
;;;; Contains: Aux. functions for cl-symbols.lsp



(declaim (optimize (safety 3)))

(defun is-external-symbol-of (sym package)
  (do-external-symbols (s package)
    (if (symbolp sym)
	(if (eq sym s) (return-from is-external-symbol-of t))
	(if (string=
	     #+lower-case sym #-lower-case (string-upcase sym)
	     (symbol-name s))
	    (return-from is-external-symbol-of t)))))

(defun test-if-not-in-cl-package (str)
  (not
   (is-external-symbol-of str 'lisp)))

(defun safe-symbol-name (sym)
  (catch-type-error (symbol-name sym)))

(defun safe-make-symbol (name)
  (catch-type-error (make-symbol name)))
