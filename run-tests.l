#!/usr/bin/env eus

;;;
;;; ANSI Common Lisp conformity test suite for EusLisp
;;;

(require :eustest "eustest.l")

;; Functions
(defun passing-tests-list (fname)
  (let* ((test-str (with-output-to-string (s) (eustest::org-level 3 s) (format s "[X] ")))
         (test-len (length test-str)))
    (with-open-file (file fname)
      (mapcan #'(lambda (str)
                  (if (and (> (length str) test-len)
                           (string= (subseq str 0 test-len) test-str))
                      (list (read-from-string (subseq str test-len)))))
              (read-lines file)))))

;; PARSE ARGUMENTS
(load "auxiliary/eus_aux/argparse.l")

(defvar argparse (instance argparse:argument-parser :init
                           :prog "run-tests.l"
                           :description "ANSI test suite for EusLisp"))
(send argparse :add-argument '("--cl-compatible" "-cl")
      :action #'(lambda () (push :cl-compatible *features*))
      :help "Set when using EusLisp cl-compatible branch")
(send argparse :add-argument '("--auxiliary" "-aux")
      :action #'(lambda () (push :cl-aux *features*))
      :help "Set to load cl-compatible libraries on 'auxiliary/cl_comp/'")
(send argparse :add-argument '("--ensure-pass" "-e")
      :dest 'infile
      :check #'probe-file
      :help "Only execute tests marked as successful in the designated log file")
(send argparse :add-argument '("--update-log" "-u")
      :dest 'outfile
      :help "Print the test results to the designated log file")
(send argparse :add-argument '("--interactive" "-i")
      :action :store-true
      :help "Set to start the euslisp interpreter with the loaded test suite")
(send argparse :add-argument '("--quiet" "-q")
      :action :store-true
      :help "Set to inhibit printing failed tests in the end")
(send argparse :parse-args)

;; PREPARE TESTS
(defvar *load-pathname* nil)
(setq call-arguments-limit 4611686018427387903)

(send (find-package "LISP") :set-val 'names (list "LISP" "CL"))
(send (find-package "USER") :set-val 'names (list "USER" "CL-USER" "COMMON-LISP-USER" "CL-TEST"))

(set-macro-character #\% nil)

(defun compile-and-load (file)
  (if (string= (subseq file 0 15) "ANSI-TESTS:AUX;")
      (load (concatenate-pathnames #.(truename #P"auxiliary/ansi_aux/") (subseq file 15)))
      (load file)))

;; LOAD CL-COMPATIBLE LIBRARIES
#+:cl-aux (load "load-cl-compatible.lsp")

;; LOAD EUS AUXILIARY
(load "auxiliary/eus_aux/eus-multiple-values.l")
(load "auxiliary/eus_aux/eus-loop.l")

;; LOAD ANSI AUXILIARY
(load "auxiliary/ansi_aux/ansi-aux.lsp")
(load "auxiliary/ansi_aux/cl-symbol-names.lsp")
(load "auxiliary/ansi_aux/universe.lsp")

;; REDEFINE DEFTEST
(defmacro deftest (name clause &rest res)
  `(progn
     (defun ,name ()
       (assert  ,(if (and (consp res) (not (cdr res))) ;; single element list
		     `(equal ,clause ',@res)
		     `(equal (multiple-value-list ,clause) ',res))))
     (send eustest::*unit-test* :add-function ',name)
     ',name))

;; LOAD TESTS
(load "load-tests.lsp")

;; RUN TESTS
(terpri *error-output*)

(unless (send argparse :interactive)
  (let* ((infile (send argparse :infile))
         (outfile (send argparse :outfile))
         (quiet (send argparse :quiet))
         (white-list (and infile (passing-tests-list infile))))

    ;; Save temporary files to `sandbox/' during test execution
    (cd "sandbox/")
    (run-all-tests :white-list white-list :exit (not outfile) :log-failures (not quiet))
    (cd "../")

    ;; Save output log
    (when outfile
      (format t "Updating '~A'...~%" outfile)
      (send eustest::*unit-test* :print-log outfile)
      (format t "... updated.~%"))

    (send eustest::*unit-test* :exit)))
