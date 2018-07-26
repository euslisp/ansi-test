(in-package "LISP")

(defun characterp (ch)
  "Returns T if ch is a character, false otherwise."
  (if (or (floatp ch) (derivedp ch object) (< ch 0))
      nil
      t))

(defun char-int (ch) (assert (characterp ch)) ch)
(defun char-not-equal (x y)
  (assert (and (characterp x) (characterp y)))
  (not (equal x y)))

(setf (symbol-function 'char<) #'<
      (symbol-function 'char=) #'=
      (symbol-function 'char>) #'>
      (symbol-function 'char/=) #'/=
      (symbol-function 'char<=) #'<=
      (symbol-function 'char>=) #'>=
      (symbol-function 'char-code) #'char-int
      (symbol-function 'char-equal) #'equal
      (symbol-function 'char-lessp) #'<
      (symbol-function 'char-greaterp) #'>
      (symbol-function 'char-not-lessp) #'>=)

(export '(characterp char-int char-code char-equal char-not-equal
          char-lessp char-greaterp char-not-lessp
          char< char= char> char/= char<= char>=))
