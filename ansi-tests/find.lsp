;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 07:49:49 2002
;;;; Contains: Tests for FIND

(in-package :cl-test)

(deftest find-list.1
  (find 'c '(a b c d e c a))
  c)

(deftest find-list.2
  (find 'c '(a b c d e c a) :from-end t)
  c)

(deftest find-list.3
  (loop for i from 0 to 7 collect
	(find 'c '(a b c d e c a) :start i))
  (c c c c c c nil nil))

(deftest find-list.4
  (loop for i from 0 to 7 collect
	(find 'c '(a b c d e c a) :start i :end nil))
  (c c c c c c nil nil))

(deftest find-list.5
  (loop for i from 7 downto 0 collect
	(find 'c '(a b c d e c a) :end i))
  (c c c c c nil nil nil))

(deftest find-list.6
  (loop for i from 0 to 7 collect
	(find 'c '(a b c d e c a) :start i :from-end t))
  (c c c c c c nil nil))

(deftest find-list.7
  (loop for i from 0 to 7 collect
	(find 'c '(a b c d e c a) :start i :end nil :from-end t))
  (c c c c c c nil nil))

(deftest find-list.8
  (loop for i from 7 downto 0 collect
	(find 'c '(a b c d e c a) :end i :from-end t))
  (c c c c c nil nil nil))

(deftest find-list.9
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find 'c '(a b c d e c a) :start i :end j)))
  ((nil nil c c c c c)
   (nil c c c c c)
   (c c c c c)
   (nil nil c c)
   (nil c c)
   (c c)
   (nil)))

(deftest find-list.10
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find 'c '(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil c c c c c)
   (nil c c c c c)
   (c c c c c)
   (nil nil c c)
   (nil c c)
   (c c)
   (nil)))

(deftest find-list.11
  (find 5 '(1 2 3 4 5 6 4 8) :key #'1+)
  4)

(deftest find-list.12
  (find 5 '(1 2 3 4 5 6 4 8) :key '1+)
  4)

(deftest find-list.13
  (find 5 '(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  4)

(deftest find-list.14
  (find 'a '(a a b a c e d a f a) :test (complement #'eq))
  b)

(deftest find-list.15
  (find 'a '(a a b a c e d a f a) :test (complement #'eq)
	    :from-end t)
  f)

(deftest find-list.16
  (find 'a '(a a b a c e d a f a) :test-not #'eq)
  b)

(deftest find-list.17
  (find 'a '(a a b a c e d a f a) :test-not 'eq
	    :from-end t)
  f)

(deftest find-list.18
  (find 'a '(a a b a c e d a f a) :test-not 'eq)
  b)

(deftest find-list.19
  (find 'a '(a a b a c e d a f a) :test-not #'eq
	    :from-end t)
  f)

(deftest find-list.20
  (find 'a '(a a b a c e d a f a) :test-not #'eq)
  b)

(deftest find-list.21
  (find 'a '(a a b a c e d a f a) :test #'eq
	    :start 2)
  a)

(deftest find-list.22
  (find 'a '(a a b a c e d a f a) :test #'eq
	    :start 2 :end nil)
  a)

(deftest find-list.23
  (find 'a '(a a b a c e d a f a) :test-not #'eq
	    :start 0 :end 5)
  b)

(deftest find-list.24
  (find 'a '(a a b a c e d a f a) :test-not #'eq
	    :start 0 :end 5 :from-end t)
  c)

(deftest find-list.25
  (find "ab" '("a" #(#\b #\a) #(#\a #\b #\c) #(#\a #\b) #(#\d #\e) f) :test #'equalp)
  #(#\a #\b))

(deftest find-list.26
  (find 'a '((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  (a b c))

(deftest find-list.27
  (find 'a '((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
	    :start 3)
  (a b))

(deftest find-list.28
  (find 'a '((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
	    :start 2 :from-end t)
  (a b))

;;; Tests on vectors

(deftest find-vector.1
  (find 'c #(a b c d e c a))
  c)

(deftest find-vector.1a
  (find 'z #(a b c d e c a))
  nil)

(deftest find-vector.2
  (find 'c #(a b c d e c a) :from-end t)
  c)

(deftest find-vector.2a
  (find 'z #(a b c d e c a) :from-end t)
  nil)

(deftest find-vector.3
  (loop for i from 0 to 7 collect
	(find 'c #(a b c d e c a) :start i))
  (c c c c c c nil nil))

(deftest find-vector.4
  (loop for i from 0 to 7 collect
	(find 'c #(a b c d e c a) :start i :end nil))
  (c c c c c c nil nil))

(deftest find-vector.5
  (loop for i from 7 downto 0 collect
	(find 'c #(a b c d e c a) :end i))
  (c c c c c nil nil nil))

(deftest find-vector.6
  (loop for i from 0 to 7 collect
	(find 'c #(a b c d e c a) :start i :from-end t))
  (c c c c c c nil nil))

(deftest find-vector.7
  (loop for i from 0 to 7 collect
	(find 'c #(a b c d e c a) :start i :end nil :from-end t))
  (c c c c c c nil nil))

(deftest find-vector.8
  (loop for i from 7 downto 0 collect
	(find 'c #(a b c d e c a) :end i :from-end t))
  (c c c c c nil nil nil))

(deftest find-vector.9
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find 'c #(a b c d e c a) :start i :end j)))
  ((nil nil c c c c c)
   (nil c c c c c)
   (c c c c c)
   (nil nil c c)
   (nil c c)
   (c c)
   (nil)))

(deftest find-vector.10
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find 'c #(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil c c c c c)
   (nil c c c c c)
   (c c c c c)
   (nil nil c c)
   (nil c c)
   (c c)
   (nil)))

(deftest find-vector.11
  (find 5 #(1 2 3 4 5 6 4 8) :key #'1+)
  4)

(deftest find-vector.12
  (find 5 #(1 2 3 4 5 6 4 8) :key '1+)
  4)

(deftest find-vector.13
  (find 5 #(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  4)

(deftest find-vector.14
  (find 'a #(a a b a c e d a f a) :test (complement #'eq))
  b)

(deftest find-vector.15
  (find 'a #(a a b a c e d a f a) :test (complement #'eq)
	    :from-end t)
  f)

(deftest find-vector.16
  (find 'a #(a a b a c e d a f a) :test-not #'eq)
  b)

(deftest find-vector.17
  (find 'a #(a a b a c e d a f a) :test-not 'eq
	    :from-end t)
  f)

(deftest find-vector.18
  (find 'a #(a a b a c e d a f a) :test-not 'eq)
  b)

(deftest find-vector.19
  (find 'a #(a a b a c e d a f a) :test-not #'eq
	    :from-end t)
  f)

(deftest find-vector.20
  (find 'a #(a a b a c e d a f a) :test-not #'eq)
  b)

(deftest find-vector.21
  (find 'a #(a a b a c e d a f a) :test #'eq
	    :start 2)
  a)

(deftest find-vector.22
  (find 'a #(a a b a c e d a f a) :test #'eq
	    :start 2 :end nil)
  a)

(deftest find-vector.23
  (find 'a #(a a b a c e d a f a) :test-not #'eq
	    :start 0 :end 5)
  b)

(deftest find-vector.24
  (find 'a #(a a b a c e d a f a) :test-not #'eq
	    :start 0 :end 5 :from-end t)
  c)

(deftest find-vector.25
  (find "ab" #("a" #(#\b #\a) #(#\a #\b #\c) #(#\a #\b) #(#\d #\e) f) :test #'equalp)
  #(#\a #\b))

(deftest find-vector.26
  (find 'a #((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  (a b c))

(deftest find-vector.27
  (find 'a #((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
	    :start 3)
  (a b))

(deftest find-vector.28
  (find 'a #((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
	    :start 2 :from-end t)
  (a b))

(deftest find-vector.29
  (let ((a (make-array '(10)
		       :initial-contents '(1 2 3 4 5 6 7 8 9 10)
		       :fill-pointer 5)))
    (loop for i from 1 to 10 collect (find i a)))
  (1 2 3 4 5 nil nil nil nil nil))

(deftest find-vector.30
  (let ((a (make-array '(10)
		       :initial-contents (loop for i from 1 for e in '(1 2 3 4 5 5 4 3 2 1)
					       collect (list e i))
		       :fill-pointer 5)))
    (loop for i from 1 to 5 collect (find i a :from-end t :key #'car)))
  ((1 1) (2 2) (3 3) (4 4) (5 5)))

;;; tests on bit vectors

(deftest find-bit-vector.1
  (find 1 #*001001010100)
  1)

(deftest find-bit-vector.1a
  (find 0 #*001001010100)
  0)

(deftest find-bit-vector.1b
  (find 2 #*001001010100)
  nil)

(deftest find-bit-vector.1c
  (find 'a #*001001010100)
  nil)

(deftest find-bit-vector.1d
  (find 1 #*000000)
  nil)

(deftest find-bit-vector.2
  (find 1 #*001001010100 :from-end t)
  1)

(deftest find-bit-vector.2a
  (find 1 #*00000 :from-end t)
  nil)

(deftest find-bit-vector.2b
  (find 0 #*00000 :from-end t)
  0)

(deftest find-bit-vector.2c
  (find 0 #*11111 :from-end t)
  nil)

(deftest find-bit-vector.2d
  (find 2 #*11111 :from-end t)
  nil)

(deftest find-bit-vector.2e
  (find 'a #*11111 :from-end t)
  nil)

(deftest find-bit-vector.3
  (loop for i from 0 to 7 collect
	(find 1 #*0010010 :start i))
  (1 1 1 1 1 1 nil nil))

(deftest find-bit-vector.4
  (loop for i from 0 to 7 collect
	(find 1 #*0010010 :start i :end nil))
  (1 1 1 1 1 1 nil nil))

(deftest find-bit-vector.5
  (loop for i from 7 downto 0 collect
	(find 1 #*0010010 :end i))
  (1 1 1 1 1 nil nil nil))

(deftest find-bit-vector.6
  (loop for i from 0 to 7 collect
	(find 1 #*0010010 :start i :from-end t))
  (1 1 1 1 1 1 nil nil))

(deftest find-bit-vector.7
  (loop for i from 0 to 7 collect
	(find 0 #*1101101 :start i :end nil :from-end t))
  (0 0 0 0 0 0 nil nil))

(deftest find-bit-vector.8
  (loop for i from 7 downto 0 collect
	(find 0 #*1101101 :end i :from-end t))
  (0 0 0 0 0 nil nil nil))

(deftest find-bit-vector.9
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find 1 #*0010010 :start i :end j)))
  ((nil nil 1 1 1 1 1)
   (nil 1 1 1 1 1)
   (1 1 1 1 1)
   (nil nil 1 1)
   (nil 1 1)
   (1 1)
   (nil)))

(deftest find-bit-vector.10
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find 1 #*0010010 :start i :end j :from-end t)))
  ((nil nil 1 1 1 1 1)
   (nil 1 1 1 1 1)
   (1 1 1 1 1)
   (nil nil 1 1)
   (nil 1 1)
   (1 1)
   (nil)))

(deftest find-bit-vector.11
  (find 2 #*00010001010 :key #'1+)
  1)

(deftest find-bit-vector.12
  (find 2 #*00010001010 :key '1+)
  1)

(deftest find-bit-vector.13
  (find 2 #*0010001000 :key #'1+ :from-end t)
  1)

(deftest find-bit-vector.14
  (find 0 #*0010111010 :test (complement #'eq))
  1)

(deftest find-bit-vector.15
  (find 0 #*0010111010 :test (complement #'eq)
	    :from-end t)
  1)

(deftest find-bit-vector.16
  (find 0 #*0010111010 :test-not #'eq)
  1)

(deftest find-bit-vector.16a
  (find 1 #*111111111111 :test-not #'eq)
  nil)

(deftest find-bit-vector.16b
  (find 0 #*0000000 :test-not #'eq)
  nil)

(deftest find-bit-vector.17
  (find 0 #*001011101 :test-not 'eq
	:from-end t)
  1)

(deftest find-bit-vector.17a
  (find 0 #*0000000 :test-not 'eq
	:from-end t)
  nil)

(deftest find-bit-vector.17b
  (find 1 #*111111111111 :test-not 'eq
	:from-end t)
  nil)

(deftest find-bit-vector.18
  (find 0 #*00101110 :test-not 'eq)
  1)

(deftest find-bit-vector.18a
  (find 0 #*00000000 :test-not 'eq)
  nil)

(deftest find-bit-vector.19
  (find 0 #*00101110 :test-not #'eq
	    :from-end t)
  1)

(deftest find-bit-vector.19a
  (find 0 #*00000000 :test-not #'eq
	:from-end t)
  nil)

(deftest find-bit-vector.20
  (find 0 #*00101110 :test-not #'eq)
  1)

(deftest find-bit-vector.21
  (find 0 #*00101110 :test #'eq
	:start 2)
  0)

(deftest find-bit-vector.21a
  (find 0 #*00111111 :test #'eq
	:start 2)
  nil)

(deftest find-bit-vector.21b
  (find 1 #*00111111 :test #'eq
	:start 2)
  1)

(deftest find-bit-vector.22
  (find 0 #*00101110 :test #'eq
	:start 2 :end nil)
  0)

(deftest find-bit-vector.22a
  (find 0 #*001111111 :test #'eq
	:start 2 :end nil)
  nil)

(deftest find-bit-vector.22b
  (find 1 #*001111111 :test #'eq
	:start 2 :end nil)
  1)

(deftest find-bit-vector.23
  (find 0 #*00101110 :test-not #'eq
	:start 0 :end 5)
  1)

(deftest find-bit-vector.23a
  (find 0 #*00000111 :test-not #'eq
	:start 0 :end 5)
  nil)

(deftest find-bit-vector.23b
  (find 0 #*00001000 :test-not #'eq
	:start 0 :end 5)
  1)

(deftest find-bit-vector.24
  (find 0 #*00101110 :test-not #'eq
	    :start 0 :end 5 :from-end t)
  1)

(deftest find-bit-vector.24a
  (find 0 #*0000001111 :test-not #'eq
	    :start 0 :end 5 :from-end t)
  nil)

(deftest find-bit-vector.24b
  (find 0 #*0000100 :test-not #'eq
	    :start 0 :end 5 :from-end t)
  1)

(deftest find-bit-vector.25
  (find 2 #*1100001010 :key #'1+
	    :start 3)
  1)

(deftest find-bit-vector.26
  (find 2 #*11100000 :key #'1+
	    :start 3)
  nil)

(deftest find-bit-vector.26a
  (find 2 #*11110000 :key #'1+
	    :start 3)
  1)

(deftest find-bit-vector.27
  (find 2 #*1100001010 :key #'1+
	    :start 2 :from-end t)
  1)

(deftest find-bit-vector.28
  (find 2 #*1100000000 :key #'1+
	    :start 2 :from-end t)
  nil)

(deftest find-bit-vector.29
  (let ((a
	 (make-array '(10) :initial-contents '(1 1 1 1 1 0 0 0 0 0)
		     :element-type 'bit
		     :fill-pointer 5)))
    (values (find 0 a)
	    (find 0 a :from-end t)))
  nil nil)

(deftest find-bit-vector.30
  (let ((a (make-array '(10) :initial-contents '(1 1 1 1 0 0 0 0 0 0)
		       :element-type 'bit
		       :fill-pointer 5)))
    (values (find 0 a) (find 0 a :from-end t)))
  0 0)

;;; strings

(deftest find-string.1
  (find #\c "abcdeca")
  #\c)

(deftest find-string.1a
  (find #\c "abCa")
  nil)

(deftest find-string.2
  (find #\c "abcdeca" :from-end t)
  #\c)

(deftest find-string.2a
  (find #\c "abCCCa" :from-end t)
  nil)

(deftest find-string.3
  (loop for i from 0 to 7 collect
	(find #\c "abcdeca" :start i))
  (#\c #\c #\c #\c #\c #\c nil nil))

(deftest find-string.4
  (loop for i from 0 to 7 collect
	(find #\c "abcdeca" :start i :end nil))
  (#\c #\c #\c #\c #\c #\c nil nil))

(deftest find-string.5
  (loop for i from 7 downto 0 collect
	(find #\c "abcdeca" :end i))
  (#\c #\c #\c #\c #\c nil nil nil))

(deftest find-string.6
  (loop for i from 0 to 7 collect
	(find #\c "abcdeca" :start i :from-end t))
  (#\c #\c #\c #\c #\c #\c nil nil))

(deftest find-string.7
  (loop for i from 0 to 7 collect
	(find #\c "abcdeca" :start i :end nil :from-end t))
  (#\c #\c #\c #\c #\c #\c nil nil))

(deftest find-string.8
  (loop for i from 7 downto 0 collect
	(find #\c "abcdeca" :end i :from-end t))
  (#\c #\c #\c #\c #\c nil nil nil))

(deftest find-string.9
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find #\c "abcdeca" :start i :end j)))
  ((nil nil #\c #\c #\c #\c #\c)
   (nil #\c #\c #\c #\c #\c)
   (#\c #\c #\c #\c #\c)
   (nil nil #\c #\c)
   (nil #\c #\c)
   (#\c #\c)
   (nil)))

(deftest find-string.10
  (loop for i from 0 to 6 collect
	(loop for j from (1+ i) to 7
	      collect
	      (find #\c "abcdeca" :start i :end j :from-end t)))
  ((nil nil #\c #\c #\c #\c #\c)
   (nil #\c #\c #\c #\c #\c)
   (#\c #\c #\c #\c #\c)
   (nil nil #\c #\c)
   (nil #\c #\c)
   (#\c #\c)
   (nil)))

(deftest find-string.11
  (find 5 "12345648" :key #'(lambda (c)
			      (1+ (read-from-string (string c)))))
  #\4)

(deftest find-string.13
  (find 5 "12345648" :key #'(lambda (c)
				  (1+ (read-from-string (string c))))
	    :from-end t)
  #\4)

(deftest find-string.14
  (find #\a "aabacedafa" :test (complement #'eq))
  #\b)

(deftest find-string.15
  (find #\a "aabacedafa" :test (complement #'eq)
	    :from-end t)
  #\f)

(deftest find-string.16
  (find #\a "aabacedafa" :test-not #'eq)
  #\b)

(deftest find-string.17
  (find #\a "aabacedafa" :test-not 'eq
	    :from-end t)
  #\f)

(deftest find-string.18
  (find #\a "aabacedafa" :test-not 'eq)
  #\b)

(deftest find-string.19
  (find #\a "aabacedafa" :test-not #'eq
	    :from-end t)
  #\f)

(deftest find-string.20
  (find #\a "aabacedafa" :test-not #'eq)
  #\b)

(deftest find-string.21
  (find #\a "aabAcedafa" :test #'char-equal
	:start 2)
  #\A)

(deftest find-string.22
  (find #\a "aabAcedafa" :test #'char-equal
	    :start 2 :end nil)
  #\A)

(deftest find-string.23
  (find #\a "aAbAcedafa" :test-not #'char-equal
	    :start 0 :end 5)
  #\b)

(deftest find-string.24
  (find #\a "aabacedafa" :test-not #'char-equal
	:start 0 :end 5 :from-end t)
  #\c)

(deftest find-string.25
  (let ((s (make-array '(10) :initial-contents "abcdefghij"
		       :element-type 'character
		       :fill-pointer 5)))
    (values
     (loop for e across "abcdefghij"
	   collect (find e s))
     (loop for e across "abcdefghij"
	   collect (find e s :from-end t))))
  (#\a #\b #\c #\d #\e nil nil nil nil nil)
  (#\a #\b #\c #\d #\e nil nil nil nil nil))

;;; Keyword tests

(deftest find.allow-other-keys.1
  (find 0 '(1 2 3 4 5) :key #'(lambda (x) (mod x 2))
	:bad t :allow-other-keys t)
  2)

(deftest find.allow-other-keys.2
  (find 0 '(1 2 3 4 5) :key #'(lambda (x) (mod x 2))
	       :allow-other-keys t :also-bad t)
  2)

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(deftest find.allow-other-keys.3
  (find 0 '(1 2 3 4 5) :key #'(lambda (x) (mod x 2))
	:allow-other-keys t
	:allow-other-keys nil
	:bad t)
  2)

(deftest find.keywords.4
  (find 2 '(1 2 3 4 5) :key #'identity :key #'1+)
  2)

(deftest find.allow-other-keys.5
  (find 'b '(nil a b c nil) :allow-other-keys nil)
  b)


;;; Error tests

(deftest find.error.1
  (classify-error (find 'a 'b))
  type-error)

(deftest find.error.2
  (classify-error (find 'a 10))
  type-error)

(deftest find.error.3
  (classify-error (find 'a 1.4))
  type-error)

(deftest find.error.4
  (classify-error (find 'e '(a b c . d)))
  type-error)

(deftest find.error.5
  (classify-error (find))
  program-error)

(deftest find.error.6
  (classify-error (find 'a))
  program-error)

(deftest find.error.7
  (classify-error (find 'a nil :bad t))
  program-error)

(deftest find.error.8
  (classify-error (find 'a nil :bad t :allow-other-keys nil))
  program-error)

(deftest find.error.9
  (classify-error (find 'a nil 1 1))
  program-error)

(deftest find.error.10
  (classify-error (find 'a nil :key))
  program-error)

(deftest find.error.11
  (classify-error (locally (find 'a 'b) t))
  type-error)

;;; Order of evaluation tests

(deftest find.order.1
  (let ((i 0) x y)
    (values
     (find (progn (setf x (incf i)) 'a)
	   (progn (setf y (incf i)) '(nil nil nil a nil nil)))
     i x y))
  a 2 1 2)

(deftest find.order.2
  (let ((i 0) a b c d e f g)
    (values
     (find (progn (setf a (incf i)) nil)
	   (progn (setf b (incf i)) '(nil nil nil a nil nil))
	   :start (progn (setf c (incf i)) 1)
	   :end   (progn (setf d (incf i)) 4)
	   :from-end (setf e (incf i))
	   :key   (progn (setf f (incf i)) #'null)
	   )
     i a b c d e f))
  a 6 1 2 3 4 5 6)

(deftest find.order.3
  (let ((i 0) a b c d e f g)
    (values
     (find (progn (setf a (incf i)) nil)
	   (progn (setf b (incf i)) '(nil nil nil a nil nil))
	   :key   (progn (setf c (incf i)) #'null)
	   :from-end (setf d (incf i))
	   :end   (progn (setf e (incf i)) 4)
	   :start (progn (setf f (incf i)) 1)
	   )
     i a b c d e f))
  a 6 1 2 3 4 5 6)
