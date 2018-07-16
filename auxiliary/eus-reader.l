(in-package :lisp)

(defun  read-from-string (string &optional (eof-error-p t) eof-value
                                          &key (start 0) (end nil)
                                          (preserve-whitespace nil))
        (let ((end (or end (length string))))
          (read (make-string-input-stream string start end))))

(export '(read-from-string))
