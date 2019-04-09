#!/usr/bin/env eus

;;;
;;; ANSI Common Lisp conformity test suite for EusLisp
;;;

(require :eustest "eustest.l")

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
(send argparse :add-argument '("--update-log" "-u")
      :dest 'logfile
      :default "REPORT.org"
      :help "Set to update log file results")
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

;; LOAD TESTS
(load "load-tests.lsp")

;; RUN TESTS
(terpri *error-output*)

;; Save temporary files to `sandbox/'
(cd "sandbox/")


(run-all-tests (not (send argparse :logfile)))

(cd "../")
(format t "Updating '~A'...~%" (send argparse :logfile))
(send eustest::*unit-test* :print-log (send argparse :logfile))
(format t "... updated.~%")
(send eustest::*unit-test* :exit)
