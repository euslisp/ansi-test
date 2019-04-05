;;;
;;; based on euslib/jsk/unittest.l by R.Ueda
;;;
;;; modified by Guilherme de Campos Affonso to:
;;; 1. Continue testing upon program errors
;;; 2. Allow to skip and test for errors
;;; 3. Modify printing
;;; special thanks to Ericles Antonio Aquiles Barbosa Lima,
;;; who joined the EusLisp ansi-test project in 2018
;;;

;; LISP OVERWRITES
(load "auxiliary/eus_aux/load" :package *lisp-package*)
(load "auxiliary/eus_aux/assert" :package *lisp-package*)

(unless (find-package "EUSTEST")
  (make-package "EUSTEST"))

(mapc #'unintern '(deftest deferror defskip run-all-tests))
(send *package* :use "EUSTEST")

(in-package "EUSTEST")

(export '(deftest deferror defskip run-all-tests))

(defun eustest-error (code msg1 form &optional (msg2))
  (if (and msg2 (zerop (length msg1))) (setq msg1 msg2 msg2 nil))
  (format *error-output* "~C[1;3~Cm~ ~A" #x1b (+ 1 48) msg1)
  (if msg2 (format *error-output* " ~A" msg2))
  (if form (format *error-output* " in ~s" form))
  (format *error-output* "~C[0m~%" #x1b)
  (reset))

(defun eustest-sigint-handler (sig code)
  (format *error-output* "eustest-sigint-handler ~A~%" sig)
  (reset))

(defmacro getstatus (status)
  `(let ((lst
          (remove-if-not #'(lambda (obj) (eql (eustest-function-status obj) ,status))
                         functions)))
     (reverse (send-all lst :name))))

;; CLASS DEFINITIONS
(defclass eustest-function
  :slots (name status dir file))
(defmethod eustest-function
  (:init (n)
    (setq name n)
    (setq dir (car *loader-current-directory*))
    (setq file *loader-current-file*)
    (setq status nil)
    self)
  (:name () name)
  (:clear () (setq status :pending))
  (:pass () (eql status :pass))
  (:increment-pass ()
    (setq status :pass)
    (format t "~A~%" name))
  (:increment-failure (&optional msg)
    (setq status :failure)
    (warning-message 1 "[ERROR] ~S" name)
    (if msg
        (warning-message 1 ": ~A" msg)
        (terpri *error-output*)))
  (:run ()
    (let* (pass
           (msg (with-output-to-string (*error-output*)
                  (setq pass (catch 0 (funcall name) t)))))
    (if pass
        (send self :increment-pass)
        (send self :increment-failure msg)))))

(defclass eustest-skip-function
  :super eustest-function
  :slots (msg))
(defmethod eustest-skip-function
  (:init (name &optional message)
    (setq msg message)
    (send-super :init name))
  (:increment-skip ()
    (setq status :skip)
    (warning-message 2 "[SKIP] ~S" name)
    (when msg
      (warning-message 2 ": ")
      (format *error-output* "~C[1;3~Cm~A~C[0m~%" #x1b 50 msg #x1b))
    (unless msg
      (terpri *error-output*)))
  (:run ()
    (send self :increment-skip)))

(defclass eustest-error-function
  :super eustest-function)
(defmethod eustest-error-function
  (:increment-pass ()
   (setq status :failure)
   (warning-message 1 "[FAIL] ~S~%" name))
  (:increment-failure (&optional msg)
     (setq status :pass)
     (format t "~A~%" name)))

(defclass eustest-container
  :super propertied-object
  :slots (functions))
(defmethod eustest-container
  (:init ()
    (setq functions nil)
    self)
  (:setup ()
    (unless (eq *error-handler* 'eustest-error)
      (setq *error-handler* 'eustest-error)
      (setq lisp::*max-callstack-depth* 0)
      (lisp::install-error-handler 'eustest-error)
      (unix:signal unix::sigint 'eustest-sigint-handler)
      (unix:signal unix::sighup 'eustest-sigint-handler)
      t))
  (:clear () (send-all functions :clear))
  (:get-function (name)
    (find-if #'(lambda (obj) (eql (send obj :name) name)) functions))
  (:all-tests () (reverse functions))
  (:all-pass () (getstatus :pass))
  (:all-failures () (getstatus :failure))
  (:all-skips () (getstatus :skip))
  (:add-function (name)
    (push (instance eustest-function :init name) functions)
    name)
  (:add-skip-function (name &optional msg)
    (push (instance eustest-skip-function :init name msg) functions)
    name)
  (:add-error-function (name)
    (push (instance eustest-error-function :init name) functions)
    name)
  (:run-all-tests ()
   (labels ((run-test (test)
              (catch 0 (send test :run) (return-from run-test))
              (send test :increment-failure)))
   (send self :setup)
   (dolist (test (send self :all-tests))
     (run-test test))
   (send self :print-result)))
  (:print-result (&optional (strm *error-output*))
    (let ((test-num (length (send self :all-tests)))
          (pass-num (length (send self :all-pass)))
          (fail-num (length (send self :all-failures)))
          (skip-num (length (send self :all-skips))))
      (format strm "~%~%~C[4;3~CmALL RESULTS:~C[0m~%" #x1b 50 #x1b)
      (format strm "  TEST-NUM: ~A~%~%" test-num)
      ;; PASS
      (format strm "  PASSED:   ~A~%" pass-num)
      ;; FAIL
      (if (> fail-num 0) (format strm "~C[3~Cm" #x1b 49))
      (format strm "  FAILURE:  ~A~%" fail-num)
      (if (> fail-num 0) (format strm "~C[0m" #x1b))
      ;; SKIP
      (if (> skip-num 0) (format strm "~C[3~Cm" #x1b 49))
      (format strm "  SKIPPED:  ~A" skip-num)
      (format strm "~C[0m~%" #x1b)
      (terpri strm)))
  (:print-log (fname) (error "TODO")))

;; INIT
(defparameter *unit-test* (instance eustest-container :init))

;; FUNCTIONS
(defun run-all-tests (&optional (exit t))
  (send *unit-test* :clear)
  (unwind-protect
       (send *unit-test* :run-all-tests)
    (when exit
      (if (or (send *unit-test* :all-failures)
              (send *unit-test* :all-skips))
          (exit 1)
          (exit 0)))))

;; MACROS
(defmacro deftest (name &rest body)
  `(progn
     (defun ,name () ,@body)
     (send *unit-test* :add-function ',name)
     ',name))

(defmacro deferror (name &rest body)
  `(progn 
     (defun ,name () ,@body)
     (send *unit-test* :add-error-function ',name)
     ',name))

(defmacro defskip (name &optional msg)
  `(send *unit-test* :add-skip-function ',name ,msg))
