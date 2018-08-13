;; Based on sbcl macro expansion
;; Implemented by furushchev and Affonso-Gui in
;; https://gist.github.com/furushchev/7865e22d23006765a5ca08e33cdeeeb9

(in-package :lisp)

(defmacro dolist (vars &rest forms)
  (let ((lists (gensym "DOLIST"))
        (loop-tag (gensym "DOLIST"))
        (maybe-decl (car forms)))
    (if (and (consp maybe-decl) (eq (car maybe-decl) 'declare))
        (setq forms (cdr forms))
        (setq maybe-decl nil))
    `(block nil
       (let ((,(car vars) nil)
             (,lists ,(cadr vars)))
         ,maybe-decl
         (tagbody ,loop-tag
            (if (endp ,lists)
		(setq ,(car vars) nil)
		(progn
		  (setq ,(car vars) (pop ,lists))
		  (tagbody
		     ,@forms
		     (go ,loop-tag)))))
         ,(caddr vars)
         ))))

(defmacro dotimes (vars &rest forms)
  (let ((endvar (gensym "DOTIMES"))
        (loop-tag (gensym "DOTIMES")))
    `(block nil
       (let ((,(car vars) 0)
             (,endvar ,(cadr vars)))
         (declare (integer ,(car vars) ,endvar))
         (tagbody ,loop-tag
            (if (> ,endvar ,(car vars))
                (tagbody
                  ,@forms
                  (setq ,(car vars) (1+ ,(car vars)))
                  (go ,loop-tag))))
         ,(caddr vars)
         ))))
