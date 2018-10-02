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

(defun code-char (n)
  "Returns char correspondent to integer n.
     In our case, chars are numbers"
  n)

(alias 'char< '<)
(alias 'char= '=)
(alias 'char> '>)
(alias 'char/= '/=)
(alias 'char<= '<=)
(alias 'char>= '>=)
(alias 'char-code 'char-int)
(alias 'char-equal 'equal)
(alias 'char-lessp '<)
(alias 'char-greaterp '>)
(alias 'char-not-lessp '>=)

(export '(characterp char-int char-code char-equal char-not-equal
          code-char char-lessp char-greaterp char-not-lessp
          char< char= char> char/= char<= char>=))
