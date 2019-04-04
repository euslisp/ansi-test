;; Shadows a few substitute functions
;; - substitute should accept a from-end keyword
;; - count = nil should default to count = 10^6 (due to hacky reasons)

(in-package :lisp)


;; TODO: implement from-end in sequence.c
(defmacro defsubstitute (name fn arg-lst
			 newitem olditem seq test testnot key iftest ifnottest start end count)  
  `(defun ,name ,arg-lst
     (let ((count (or (and count (< count 0) 0) count 1000000))
	   len)
       (unless end (setq len (length seq) end len))
       (if from-end
	   (let ((len (or len (length seq))))
	     (reverse (,fn ,newitem ,olditem (reverse ,seq) ,test ,testnot ,key ,iftest ,ifnottest (- len ,end) (- len ,start) ,count)))
	   (,fn ,newitem ,olditem ,seq ,test ,testnot ,key ,iftest ,ifnottest ,start ,end ,count)))))

(defsubstitute substitute system::raw-substitute
  (newitem olditem seq 
	   &key (start 0) (end nil)
	   (test #'eq) (test-not nil)
	   (count 1000000)
	   (key #'identity)
	   (from-end nil))
  newitem olditem seq test test-not key nil nil start end count)

(defsubstitute substitute-if system::raw-substitute
  (newitem pred seq
	   &key (start 0) (end nil)
	   (count 1000000)
	   (key #'identity)
	   (from-end nil))
  newitem nil seq nil nil key pred nil start end count)

(defsubstitute substitute-if-not system::raw-substitute
  (newitem pred seq
	   &key (start 0) (end nil)
	   (count 1000000)
	   (key #'identity)
	   (from-end nil))
  newitem nil seq nil nil key nil pred start end count)

(defsubstitute nsubstitute system::raw-nsubstitute
  (newitem olditem seq
	   &key (start 0) (end nil)
	   (test #'eq) (test-not nil)
	   (count 1000000)
	   (key #'identity)
	   (from-end nil))
  newitem olditem seq test test-not key nil nil start end count)

(defsubstitute nsubstitute-if system::raw-nsubstitute
  (newitem pred seq
	   &key (start 0) (end nil)
	   (key #'identity)
	   (count 1000000)
	   (from-end nil))
  newitem nil seq nil nil key pred nil start end count)

(defsubstitute nsubstitute-if-not system::raw-nsubstitute
  (newitem pred seq
	   &key (start 0) (end nil)
	   (count 1000000)
	   (key #'identity)
	   (from-end nil))
  newitem nil seq nil nil key nil pred start end count)

;; ;; Shadows replace function
;; ;; - replace should default nil to entire string

(defun replace (dest src &key (start1 0) (end1 nil)
                              (start2 0) (end2 nil))
  (let ((end1 (or end1 (length dest)))
        (end2 (or end2 (length src))))
      (let ((result dest) (count (min (- end1 start1) (- end2 start2))))
        (cond ((listp dest)
               (setq dest (nthcdr start1 dest))
               (cond ((listp src)
                      (setq src (nthcdr start2 src))
                      (dotimes (c count)
                        (setq (dest . car) (pop src))
                        (pop dest)))
                     (t
                      (dotimes (c count)
                        (setq (dest . car) (aref src start2))
                        (inc start2) (pop dest)))))
              ((listp src)	; list --> vector
               (setq src (nthcdr start2 src))
               (dotimes (c count)
                 (aset dest start1 (pop src))
                 (inc start1)))
              (t (system::vector-replace dest src start1 end1 start2 end2)))
        result)))


;; Add mismatch function

(defmacro mismatch-core (from-end)
  `(when (if test-not
	     (funcall test-not
		      (funcall key (elt seq1 index1))
		      (funcall key (elt seq2 index2)))
	     (not (funcall test
			   (funcall key (elt seq1 index1))
			   (funcall key (elt seq2 index2)))))
     (return-from mismatch ,(if from-end
				'(1+ index1)
				'index1))))

(defun mismatch (seq1 seq2 &key (from-end nil)
                                (test #'eql)
                                (test-not nil)
                                (start1 0)
                                (start2 0)
                                (end1 nil)
                                (end2 nil)
                                (key #'identity))
  "The specified subsequences of Sequence1 and Sequence2 are compared
   element-wise.  If they are of equal length and match in every element, the
   result is NIL.  Otherwise, the result is a non-negative integer, the index
   within Sequence1 of the leftmost position at which they fail to match; or,
   if one is shorter than and a matching prefix of the other, the index within
   Sequence1 beyond the last position tested is returned.  If a non-Nil
   :From-End keyword argument is given, then one plus the index of the
   rightmost position in which the sequences differ is returned."
  (let ((end1 (or end1 (length seq1)))
        (end2 (or end2 (length seq2))))
    (if (or (zerop end1) (zerop end2)) (return-from mismatch 0))
    (if from-end
	(loop
	   for index1 from (- end1 1) downto start1
	   for index2 from (- end2 1) downto start2
	 do
	     (mismatch-core t))
	(loop
	   for index1 from start1 below end1
	   for index2 from start2 below end2
	 do
	     (mismatch-core nil)))
    (cond
      ((> (- end1 start1) (- end2 start2)) end2)
      ((< (- end1 start1) (- end2 start2)) end1)
      (t nil))))

;; ;; Auxiliary function to shadow peek-char
;; (defun OOpeek-char (&optional (stream *standard-input*)
;;                              (eof-errorp t) eof-value recursive-p)
;;   (peek-char stream eof-errorp eof-value recursive-p))

(export '(mismatch))
