#!/usr/bin/env eus
(require :unittest "lib/llib/unittest.l")

;; LOAD CL-COMPATIBLE LIBRARIES
#+:cl-aux (load "load-cl-compatible.lsp")

;; LOAD EUS AUXILIARY
(load "auxiliary/eus_aux/eus-multiple-values.l")
(load "auxiliary/eus_aux/eus-loop.l")

(defvar *signals-error* t)

;; OVERWRITES

(defun init-unit-test ()
  (lisp::install-error-handler 'unittest-error)
  (unix:signal unix::sigint 'unittest-sigint-handler)
  (unix:signal unix::sighup 'unittest-sigint-handler)
  t)

(defun unittest-error (code msg1 form &optional (msg2))
  (when *signals-error*
    ;; set failure on errors
    (send *unit-test* :increment-failure code msg1 nil)

    (format *error-output* "~C[1;3~Cm~ ~A" #x1b (+ 1 48) msg1)
    (if msg2 (format *error-output* " ~A" msg2))
    (if form (format *error-output* " in ~s" form))
    (format *error-output* "~C[0m~%" #x1b))
  (reset))

(defun unittest-sigint-handler (sig code)
  (format *error-output* "unittest-sigint-handler ~A~%" sig)
  (reset))

(defun run-test (func-sym)
  (catch 0 (=run-test func-sym) (return-from run-test))
  (unless (unit-test-result-failures (car (send *unit-test* :result)))
    (catch 0 (error "fatal error")))) ;; catch errors

;; PRINTING RELATED
(defun =run-test (func-sym)
  (let ((func (symbol-function func-sym)) tm)
    ;; initilize result
    (send *unit-test* :init-result func-sym)
    (send *unit-test* :increment-test func-sym)
    (setq tm (now))
    (funcall func)
    (if (plusp (send (car (send *unit-test* :result)) :num-successes))
      (format #.*standard-output* "~A~%" func-sym))
    (send *unit-test* :set-time-to-current-result (send (now) :elapsed tm))))

(defclass unit-test-result
  :super propertied-object
  :slots (name tests failures skips time))
(defmethod unit-test-result
  (:num-skips () (if skips 1 0))
  (:num-successes () (- (send self :num-tests)
                        (send self :num-failures)
                        (send self :num-skips))))

(defmethod unit-test-container
  (:print-normal-result (&optional (strm *error-output*))
    (let ((all-tests (apply #'+ (send-all result :num-tests)))
	  (all-successes (apply #'+ (send-all result :num-successes)))
	  (all-failures (apply #'+ (send-all result :num-failures)))
	  (all-skips (apply #'+ (send-all result :num-skips))))
      (format strm "~%~%~C[4;3~CmALL RESULTS:~C[0m~%" #x1b 50 #x1b)
      (format strm "  TEST-NUM: ~A~%~%" all-tests)
      ;; PASS
      (format strm "  PASSED:   ~A~%" all-successes)
      ;; FAIL
      (if (> all-failures 0) (format strm "~C[3~Cm" #x1b 49))
      (format strm "  FAILURE:  ~A~%" all-failures)
      (if (> all-failures 0) (format strm "~C[0m" #x1b))
      ;; SKIP
      (if (> all-skips 0) (format strm "~C[3~Cm" #x1b 49))
      (format strm "  SKIPPED:  ~A~%" all-skips)
      (format strm "~C[0m~%" #x1b)

      ;; Print all failures
      ;; (dolist (r (reverse (remove-if-not #'(lambda (obj) (unit-test-result-failures obj)) result)))
      ;;   (warning-message 1 "~A " (unit-test-result-name r)))
      ;; (format strm "~%~%")
      ))
  (:increment-failure (test msg trace)
   (when result
     (push (list test msg trace) (unit-test-result-failures (car result)))
     (warning-message 1 "[ERROR] At ~S: " (unit-test-result-name (car result)))
     (when trace
       (format *error-output* "~C[3~Cmassert failed in ~A.~C[0m~%" #x1b 49 test #x1b))))
  (:increment-skip (name &optional msg)
    (when result
      (setf (unit-test-result-skips (car result)) name)
      (warning-message 2 "[SKIP] ~S" name)
      (when msg
        (warning-message 2 ": ")
        (format *error-output* "~C[1;3~Cm~A~C[0m~%" #x1b 50 msg #x1b))
      (terpri *error-output*))))

(defmacro deftest (name clause &rest res)
  `(progn
     (defun ,name ()
       (assert  ,(if (and (consp res) (not (cdr res)))
		     `(equal ,clause ',@res)
		     `(equal (multiple-value-list ,clause) ',res))))
     (send *unit-test* :add-function ',name)
     ',name))

(defmacro defskip (name &optional msg)
  `(progn
     (defun ,name () (send *unit-test* :increment-skip ',name ,msg))
     (send *unit-test* :add-function ',name)
     ',name))


;; PREPARE TESTS

(defvar *load-pathname* nil)
(setq lisp::*max-callstack-depth* 0
      call-arguments-limit 4611686018427387903)

(send (find-package "LISP") :set-val 'names (list "LISP" "CL"))
(send (find-package "USER") :set-val 'names (list "USER" "CL-USER" "COMMON-LISP-USER" "CL-TEST"))

(set-macro-character #\% nil)

(defun compile-and-load (file)
  (if (string= (subseq file 0 15) "ANSI-TESTS:AUX;")
      (load (concatenate-pathnames #.(truename #P"auxiliary/ansi_aux/") (subseq file 15)))
      (load file)))

(defmacro assert (pred &optional (message "") &rest args)
  (let ((ret (gensym)))
    `(let* (failure (ret ,pred))
       (if (not ret)
	   ;; escape <> for xml
	   (send *unit-test* :increment-failure ',pred (format nil ,message ,@args) nil)))))

;; *UNIT-TEST*
(setq *unit-test* (instance unit-test-container :init))

;; LOAD ANSI AUXILIARY
(load "auxiliary/ansi_aux/ansi-aux.lsp")
(load "auxiliary/ansi_aux/cl-symbol-names.lsp")
(load "auxiliary/ansi_aux/universe.lsp")

;; LOAD TESTS
(load "load-tests.lsp")

;; RUN TESTS
(init-unit-test)

(terpri *error-output*)

;; Save temporary files to `sandbox/'
(cd "sandbox/")

;; Set `*standard-output*' exclusively for printing successfull tests,
;; which is used for generating test reports
(let ((*standard-output* *error-output*))
  (run-all-tests))
