(require :unittest "lib/llib/unittest.l")

(defun single (lst) (and (consp lst) (not (cdr lst))))
(defun sym-class (type)
  (cond
    ((eql type 'list) cons)
    ((symbolp type) (symbol-value type))
    (t type)))

;; OVERWRITES

(defun unittest-error (code msg1 form &optional (msg2))
  (format *error-output* "~C[1;3~Cm~A unittest-error: ~A" #x1b (+ 1 48) *program-name* msg1)
  (if msg2 (format *error-output* " ~A" msg2))
  (if form (format *error-output* " in ~s" form))

  ;; set failure on errors
  (send *unit-test* :increment-failure code msg1 nil)
  (format *error-output* "~C[0m~%" #x1b)
  (reset))

(defun unittest-sigint-handler (sig code)
  (format *error-output* "unittest-sigint-handler ~A~%" sig)
  (reset))

(setf (symbol-function '=run-test) #'run-test)
(defun run-test (func-sym)
  (catch 0 (=run-test func-sym) (return-from run-test))
  (unless (unit-test-result-failures (car (send *unit-test* :result)))
    (catch 0 (error "fatal error")))) ;; catch errors

(defmethod unit-test-container
  (:print-normal-result (&optional (strm t))
    (let ((all-tests (apply #'+ (send-all result :num-tests)))
	  (all-successes (apply #'+ (send-all result :num-successes)))
	  (all-failures (apply #'+ (send-all result :num-failures))))
      (warning-message 2 "~%ALL RESULTS:~%")
      (format strm "  TEST-NUM: ~A~%" all-tests)
      (format strm "  PASSED:   ~A~%" all-successes)
      (if (> all-failures 0) (format strm "~C[3~Cm" #x1b 49))
      (format strm "  FAILURE:  ~A~%~%" all-failures)
      (if (> all-failures 0) (format strm "~C[0m" #x1b))
      ;; only print failures
      (dolist (r (reverse (remove-if-not #'(lambda (obj) (unit-test-result-failures obj)) result)))
	(warning-message 1 "~A " (unit-test-result-name r)))
      (format strm "~%~%"))))

(defmacro deftest (name clause &rest res)
  `(progn
     (defun ,name ()
       (assert  ,(if (single res)
		     `(equal ,clause ',@res)
		     `(equal ,clause (apply #'values ',res)))))
     (send *unit-test* :add-function ',name)
     ',name))


;; PREPARE TESTS

(init-unit-test)

(defvar *load-pathname* nil)
(setq *error-handler* 'unittest-error
      *exit-on-fatal-error* nil
      lisp::*max-callstack-depth* 0
      call-arguments-limit 4611686018427387903)

(send (find-package "LISP") :set-val 'names (list "LISP" "CL"))
(send (find-package "USER") :set-val 'names (list "USER" "CL-TEST"))


(set-macro-character #\% nil)
(set-dispatch-macro-character #\# #\C #'(lambda (s n c)
					  (multiple-value-bind (r i) (read s)
					    (complex r i))))
(defun complex (real img) (make-instance complex :real real :imaginary img))

(defun find-class (obj &optional error-p) (class obj))
(defun compile-and-load (file)
  (if (string= (subseq file 0 15) "ANSI-TESTS:AUX;")
      (load (concatenate string "../auxiliary/" (subseq file 15)))
      (load file)))

(setf (symbol-function '=concatenate) #'concatenate)
(defun concatenate (type &rest args)
  (apply #'=concatenate (sym-class type) args))

(setf (symbol-function '=coerce) #'coerce)
(defun coerce (obj type)
  (=coerce obj (sym-class type)))

(setf (symbol-function '=in-package) #'in-package)

(defmacro ignore (&rest syms)
  `(progn
     ,@(mapcar #'(lambda (sym) `(defmacro ,sym (&rest args))) syms)))

(ignore in-package declaim handler-bind report-and-ignore-errors ignore-errors)

(load "auxiliary/eus-loop.l")
(load "auxiliary/ansi-aux.lsp")
(load "auxiliary/random-aux.lsp")

(load "symbols/load.lsp")
(load "eval-and-compile/load.lsp")
(load "data-and-control-flow/load.lsp")
(load "iteration/load.lsp")
;; (load "objects/load.lsp") **UNSUPPORTED**
(load "conditions/load.lsp")
(load "cons/load.lsp")

(run-all-tests)
