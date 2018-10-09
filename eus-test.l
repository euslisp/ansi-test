#!/usr/bin/env eus
(require :unittest "lib/llib/unittest.l")

;; LOAD CL-COMPATIBLE LIBRARIES
(load "auxiliary/eus-character.l")
(load "auxiliary/eus-iteration.l")
(load "auxiliary/eus-predicates.l")
(load "auxiliary/eus-sequences.l")
(load "auxiliary/eus-declarations.l")
(load "auxiliary/eus-numbers.l")
(load "auxiliary/eus-types.l")
(load "auxiliary/eus-printer.l")
(load "auxiliary/eus-handlers.l")
(load "auxiliary/eus-packages.l")

;; LOAD EUS AUXILIARY
(load "auxiliary/eus-multiple-values.l")
(load "auxiliary/eus-loop.l")

(defvar *signals-error* t)

(defun single (lst) (and (consp lst) (not (cdr lst))))

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
    (format t "~A~%" func-sym)
    (send *unit-test* :set-time-to-current-result (send (now) :elapsed tm))))

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
      (format strm "~%~%")))
  (:increment-failure
   (test msg trace)
   (when result
     (push (list test msg trace) (unit-test-result-failures (car result)))
     (warning-message 1 "[ERROR] At ~S: " (unit-test-result-name (car result)))
     (when trace
       (format *error-output* "~C[3~Cmassert failed in ~A.~C[0m~%" #x1b 49 test #x1b)))))

(defmacro deftest (name clause &rest res)
  `(progn
     (defun ,name ()
       (assert  ,(if (single res)
		     `(equal ,clause ',@res)
		     `(equal (multiple-value-list ,clause) ',res))))
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
      (load (concatenate string "../auxiliary/" (subseq file 15)))
      (load file)))

(alias '=in-package 'in-package)

(defmacro assert (pred &optional (message "") &rest args)
  (let ((ret (gensym)))
    `(let* (failure (ret ,pred))
       (if (not ret)
	   ;; escape <> for xml
	   (send *unit-test* :increment-failure ',pred (format nil ,message ,@args) nil)))))

;; *UNIT-TEST*
(setq *unit-test* (instance unit-test-container :init))


;; LOAD AUXILIARY
(load "auxiliary/ansi-aux.lsp")
(load "cl-symbol-names.lsp")
(load "universe.lsp")

;; LOAD TESTS
(load "symbols/load.lsp")
(load "eval-and-compile/load.lsp")
(load "data-and-control-flow/load.lsp")
(load "iteration/load.lsp")
(load "conditions/load.lsp")
(load "cons/load.lsp")
(load "strings/load.lsp")
(load "characters/load.lsp")
(load "pathnames/load.lsp")
(load "files/load.lsp")
(load "hash-tables/load.lsp")
(load "arrays/load.lsp")
(load "environment/load.lsp")
(load "misc/load.lsp")
(load "numbers/load.lsp")
(load "printer/load.lsp")
(load "reader/load.lsp")

(load "packages/load.lsp")
(load "streams/load.lsp")
(load "sequences/load.lsp")
(load "system-construction/load.lsp")
(load "structures/load.lsp")
;; (load "types-and-classes/load.lsp")
;; (load "objects/load.lsp") **UNSUPPORTED**

;; RUN TESTS
(init-unit-test)

(terpri)
(run-all-tests)
