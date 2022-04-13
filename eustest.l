;;;
;;; based on euslib/jsk/unittest.l by R.Ueda
;;;
;;; modified by Guilherme de Campos Affonso to:
;;; 1. Continue testing upon program errors
;;; 2. Allow to skip and test for errors
;;; 3. Track test files
;;; 4. Modify printing and logging
;;; special thanks to Ericles Antonio Aquiles Barbosa Lima,
;;; who joined the EusLisp ansi-test project in 2018
;;;

;; LISP OVERWRITES
(load "auxiliary/eus_aux/assert" :package *lisp-package*)
(load "auxiliary/eus_aux/ratio" :package *lisp-package*)
(load "auxiliary/eus_aux/load" :package *lisp-package*)


(unless (find-package "EUSTEST")
  (make-package "EUSTEST"))

(mapc #'unintern '(deftest deferror defskip run-all-tests))
(send *package* :use "EUSTEST")

(in-package "EUSTEST")

(export '(deftest deferror defskip run-all-tests))

;; HANDLERS
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

;; UTILITY
(defun getproperty (list place val &key (test #'eql))
  (remove-if-not #'(lambda (obj) (funcall test (send obj :get-val place) val))
                 list))

(defun org-level (n stream)
  (spaces (* 2 n) stream)
  (format stream "- "))

(defun ensure-directory-namestring (file)
  (or (directory-namestring file) "./"))

(defun file-namestring (file)
  (let* ((path (pathname file))
         (name (send path :name)))
    (if name
        (namestring
         (make-pathname :name (send path :name)
                        :type (send path :type)))
        (car (last (send path :directory))))))

(defun childpath-p (child-path parent-path)
  (flet ((get-dir (path) (send (truename path) :directory)))
    (let ((child-dir (get-dir child-path))
          (parent-dir (get-dir parent-path)))
      (and (>= (length child-dir) (length parent-dir))
           (every #'identity
                  (mapcar #'equal parent-dir child-dir))))))

(defmacro maybe-print-log-line (type lvl &optional key)
  (let ((val (gensym)))
    `(let ((,val ,(if key `(,key (eustest-function-file test))
                    '(eustest-function-file test))))
       (unless (string= ,type ,val)
         (setq ,type ,val)
         (org-level ,lvl log)
         (format log "[[~A][~A]] [~A]~%"
                 ,type
                 (file-namestring ,type)
                 (gethash ,type totals))))))

;; CLASS DEFINITIONS
(defclass eustest-function
  :slots (name status file))
(defmethod eustest-function
  (:init (n)
    (setq name n)
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
  (:run (&key compile)
    (if compile (compile name))
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
  (:run (&key compile)
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
  (:all-tests () (reverse functions))
  (:all-pass () (getproperty (send self :all-tests) 'status :pass))
  (:all-failures () (getproperty (send self :all-tests) 'status :failure))
  (:all-skips () (getproperty (send self :all-tests) 'status :skip))
  (:get-function (name)
    (find-if #'(lambda (obj)
                 (if (stringp name)
                     (string-equal (symbol-pname (send obj :name)) name)
                     (eql (send obj :name) name)))
             functions))
  (:get-tests (&rest names)
    (let (acc)
      (dolist (nm names)
        (dolist (test (send self :all-tests))
          (if (string-equal nm (subseq (string (send test :name)) 0 (length nm)))
              (push (send test :name) acc))))
      (nreverse acc)))
  (:get-directory-tests (name)
    (unless (= (elt name (1- (length name))) #\/)
      (setq name (concatenate string name "/")))
    (getproperty (send self :all-tests) 'file name :test #'childpath-p))
  (:get-file-tests (name)
    (let ((name (namestring (truename name))))
      (getproperty (send self :all-tests) 'file name :test #'string=)))
  (:get-totals ()
    ;; emacs can also calculate the totals in the .org file by using
    ;; `(let (org-checkbox-hierarchical-statistics) (org-update-checkbox-count t))'
    ;; calculations are coded in here for performance reasons
    (let ((hash (make-hash-table :test #'string=)))
      (flet ((incf-hash (key obj)
               (unless (gethash key hash) (setf (gethash key hash) (instance ratio :init 0 0)))
               (incf (ratio-denominator (gethash key hash)))
               (if (send obj :pass) (incf (ratio-numerator (gethash key hash))))))
        (mapc #'(lambda (obj)
                  (let* ((file (eustest-function-file obj))
                         (dir (ensure-directory-namestring file)))
                    ;; Directory count
                    (incf-hash dir obj)
                    ;; File count
                    (if file (incf-hash file obj))))
              functions)
        hash)))
  (:add-function (name)
    (push (instance eustest-function :init name) functions)
    name)
  (:add-skip-function (name &optional msg)
    (push (instance eustest-skip-function :init name msg) functions)
    name)
  (:add-error-function (name)
    (push (instance eustest-error-function :init name) functions)
    name)
  (:run-all-tests (&key white-list black-list compile)
   (labels ((run-test (test)
              (catch 0
                (send test :run :compile compile)
                (return-from run-test))
              (send test :increment-failure)))
   (send self :setup)
   (when (stringp (car white-list))
     (setq white-list (send* self :get-tests white-list))
     (unless white-list
       (warning-message 1 "No tests found!~%")
       (exit 1)))
   (if black-list (setq black-list (send* self :get-tests black-list)))
   (dolist (test (send self :all-tests))
     (if (and (or (null white-list) (memq (send test :name) white-list))
              (not (memq (send test :name) black-list)))
         (run-test test)))
   (send self :print-result)))
  (:print-result (&optional (strm *error-output*))
    (let* ((pass-num (length (send self :all-pass)))
           (fail-num (length (send self :all-failures)))
           (skip-num (length (send self :all-skips)))
           (test-num (+ pass-num fail-num skip-num)))
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
  (:print-log (fname)
    (with-open-file (log fname :direction :output :if-exists :supersede)
      ;; print headings
      (format log "# USAGE~%")
      (format log "These .org files are intended to be used with the emacs text editor.~%")
      (format log "Use *TAB* to FOLD or UNFOLD a list or sublist.~%")
      (format log "Use *C-c C-o* with the cursor over a test file to open it~%")
      (format log "An 'X' means the test succeed.~%~%")
      (org-level 0 log)
      (format log "Ansi-tests [~2,1F%]~%"
              (* 100 (/ (float (length (send self :all-pass)))
                        (length (send self :all-tests)))))

      ;; print data
      (let ((totals (send self :get-totals))
            dir file)
        (dolist (test (send self :all-tests))
          (maybe-print-log-line dir 1 ensure-directory-namestring)
          (maybe-print-log-line file 2)
          (org-level 3 log)
          (format log "[~C] ~A~%"
                  (if (send test :pass) #\X #\Space)
                  (send test :name))))))
  (:exit ()
    (if (or (send self :all-failures) (send self :all-skips))
        (exit 1)
        (exit 0))))

;; INIT
(defparameter *unit-test* (instance eustest-container :init))

;; FUNCTIONS
(defun run-all-tests (&key white-list black-list compile exit (log-failures t))
  (send *unit-test* :clear)
  (send *unit-test* :run-all-tests :white-list white-list :black-list black-list
        :compile compile)
  (when log-failures
    (dolist (f (send *unit-test* :all-failures))
      (warning-message 1 "~A " (send f :name)))
    (terpri *error-output*))
  (if exit (send *unit-test* :exit)))

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
