;; Shadows a few substitute functions
;; - substitute should accept a from-end keyword
;; - count = nil should default to count = 10^6 (due to hacky reasons)

(in-package :lisp)


(defun substitute (newitem olditem seq &key (start 0)
                                         (end (length seq))
                                         (test #'eq)
                                         (test-not nil)
                                         (count 1000000)
                                         (key #'identity)
                                         (from-end nil))
  (let ((count (or (and count (< 0 count) 0) count 1000000)))
    (if from-end
        (reverse (system::raw-substitute newitem olditem (reverse seq) test test-not key nil nil start end count))
        (system::raw-substitute newitem olditem seq test test-not key nil nil start end count))))

(defun substitute-if (newitem pred seq &key (start 0)
                                         (end (length seq))
                                         (count 1000000)
                                         (key #'identity)
                                         (from-end nil))
  (let ((count (or (and count (< 0 count) 0) count 1000000)))
    (if from-end
        (reverse (system::raw-substitute newitem nil (reverse seq) nil nil key pred nil start end count))
        (system::raw-substitute newitem nil seq nil nil key pred nil start end count))))

(defun substitute-if-not (newitem pred seq &key (start 0)
                                             (end (length seq))
                                             (count 1000000)
                                             (key #'identity)
                                             (from-end nil))
  (let ((count (or (and count (< 0 count) 0) count 1000000)))
    (if from-end
        (reverse (system::raw-substitute newitem nil (reverse seq) nil nil key nil pred start end count))
        (system::raw-substitute newitem nil seq nil nil key nil pred start end count))))


(defun nsubstitute (newitem olditem seq &key (start 0)
                                          (end (length seq))
                                          (test #'eq)
                                          (test-not nil)
                                          (count 1000000)
                                          (key #'identity)
                                          (from-end nil))
  (let ((count (or (and count (< 0 count) 0) count 1000000)))
    (if from-end
        (reverse (system::raw-nsubstitute newitem olditem (reverse seq) test test-not key nil nil start end count))
        (system::raw-nsubstitute newitem olditem seq test test-not key nil nil start end count))))

(defun nsubstitute-if (newitem pred seq &key (start 0)
                                          (end (length seq))
                                          (nil)
                                          (key #'identity)
                                          (from-end nil))
  (let ((count (or (and count (< 0 count) 0) count 1000000)))
    (if from-end
        (reverse (system::raw-nsubstitute newitem nil (reverse seq) nil nil key pred nil start end count))
        (system::raw-nsubstitute newitem nil seq nil nil key pred nil start end count))))

(defun nsubstitute-if-not (newitem pred seq &key (start 0)
                                              (end (length seq))
                                              (count 1000000)
                                              (key #'identity)
                                              (from-end nil))
  (let ((count (or (and count (< 0 count) 0) count 1000000)))
    (if from-end
        (reverse (system::raw-nsubstitute newitem nil (reverse seq) nil nil key nil pred start end count))
        (system::raw-nsubstitute newitem nil seq nil nil key nil pred start end count))))

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

(defun mismatch (seq1 seq2 &key (from-end nil)
                                (test #'eql)
                                (start1 0)
                                (start2 0)
                                (end1 nil)
                                (end2 nil))
  (let ((end1 (or end1 (length seq1)))
        (end2 (or end2 (length seq2)))
        (seq1 (or (and from-end (reverse seq1)) seq1))
        (seq2 (or (and from-end (reverse seq2)) seq2)))
    (progn
      (loop
         for index1 from start1 to (- end1 1)
         for index2 from start2 to (- end2 1)
         do
           (when (not (funcall test (elt seq1 index1) (elt seq2 index2)))
             (return-from mismatch index1))
      )
      (if (> (- end1 start1) (- end2 start2))
          end1
          nil))))


;; Shadows peek-char to accept peek-type, and ignore it for now
(defun peek-char (&optional (peek-type nil) (stream *standard-input*)
                            (eof-errorp t) eof-value recursive-p)
  (if (streamp peek-type)
      (peek-char peek-type stream eof-errorp)
      (peek-char stream eof-errorp eof-value)))

(export 'mismatch)
