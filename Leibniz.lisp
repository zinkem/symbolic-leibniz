;smatch 1, no wildcard
(defun smatch1 (p s)
  (cond
   ((null p) (null s)) 
   ((null s) nil)
   ((atom p) (eq p s))
   ((atom s) nil)
   (t (and (smatch1 (car p) (car s))
	   (smatch1 (cdr p) (cdr s)) )) ) )

;smatch2 with wild card matching added
(defun smatch2 (p s)
  (cond
   ((null p) (null s)) 
   ((null s) nil)
   ((atom p) (or (eq p s)
		 (equal p '?) ))
   ((atom s) nil)
   (t (and (smatch2 (car p) (car s))
	   (smatch2 (cdr p) (cdr s)) )) ) )

;final smatch with hashtable storage and return
(defun smatch (p s &optional (ht (make-hash-table)))
  (cond
   ((null p) (if (null s) ht))
   ((null s) nil)
   ((atom p) (if (eq p s) ht))
   ((equal (car p) '?) (if (setf (gethash (cadr p) ht) s) ht))
   ((atom s) nil)
   (t (and (smatch (car p) (car s) ht)
	   (smatch (cdr p) (cdr s) ht) )) ) )

;displays contents of hash table
(defun phash (ht)
  (maphash #'(lambda (k v) (format t "~A=~A~%" k v)) ht) )


;these functions helpful for referring to productions
(defun rule-test (rule) (car rule))
(defun rule-xform (rule) (cadr rule))
(defun rule-name (rule) (caddr rule))

(defun test (rule func ht)
  (if (funcall (rule-test rule) func ht) 
      (funcall (rule-xform rule) ht) ) )

;productions for differentiation
(defparameter *diff-sum-rule*
  (list
   #'(lambda (f ht)
       (if (smatch '(d (+ (? u) (? v)) (? x)) f ht) t) )
   #'(lambda (ht)
       (let ((u (gethash 'u ht))
	     (v (gethash 'v ht))
	     (x (gethash 'x ht)))
	 (list '+ 
	       (list 'd u x) 
	       (list 'd v x))))
   '*diff-sum-rule*) )

(defparameter *diff-x-rule*
  (list
   #'(lambda (f ht)
       (and
	(smatch '(d (? e) (? v)) f ht)
	(equal (gethash 'e ht) (gethash 'v ht)) ) )
   #'(lambda (ht) 1)
   '*diff-x-rule*) )


(defparameter *diff-const-rule*
  (list 
   #'(lambda (f ht)
       (and
	(smatch '(d (? c) (? v)) f ht)
	(numberp (gethash 'c ht)) ) )
   #'(lambda (ht) 0)
   '*diff-const-rule*) )

(defparameter *diff-product-rule*
  (list
   #'(lambda (f ht) 
       (if (smatch '(d (* (? u) (? v)) (? x)) f ht) t) )
   #'(lambda (ht) 
       (let ((u (gethash 'u ht))
	     (v (gethash 'v ht))
	     (x (gethash 'x ht)) )
	 (list '+ 
	       (list '*
		     u
		     (list 'd v x) )
	       (list '*
		     v
		     (list 'd u x) ) ) ) )
   '*diff-product-rule*) )

(defparameter *diff-power-rule*
  (list 
   #'(lambda (f ht)
       (and
	(smatch '(d (expt (? e) (? n)) (? v)) f ht)
	(numberp (gethash 'n ht)) ) ) 
   #'(lambda (ht)
       (let ((e (gethash 'e ht))
	     (n (gethash 'n ht))
	     (v (gethash 'v ht)) )
	 (list '* n
	       (list '* (list 'expt e (- n 1))
		     (list 'd e v) ) ) ) )
   '*diff-power-rule*))

(defparameter *expt0-rule*
  (list
   #'(lambda (f ht)
       (if (smatch '(expt (? x) 0) f ht) t) )
   #'(lambda (ht) 1)
   '*expt0-rule*) )


(defparameter *expt1-rule*
  (list
   #'(lambda (f ht)
       (if (smatch '(expt (? x) 1) f ht) t) )
   #'(lambda (ht) 
       (let ((x (gethash 'x ht)))
	 x) )
   '*expt1-rule*) )


(defparameter *unity-rule*
  (list
   #'(lambda (f ht)
       (or (or (smatch '(* (? x) 1) f ht)
	       (smatch '(* 1 (? x)) f ht) )
	   (or (smatch '(+ (? x) 0) f ht)
	       (smatch '(+ 0 (? x)) f ht) ) ) )
   #'(lambda (ht) (gethash 'x ht))
   '*unity-rule*) )


(defparameter *times0-rule*
  (list
   #'(lambda (f ht)
       (or (smatch '(* (? 0) 0) f ht)
	   (smatch '(* 0 (? y)) f ht) ) )
   #'(lambda (ht) 0) 
   '*times0-rule*) )

(defparameter *fold-binop-rule*
  (list
   #'(lambda (f ht)
       (and (smatch '((? o) (? l) (? r)) f ht)
	    (numberp (gethash 'l ht))
	    (numberp (gethash 'r ht)) ) )
   #'(lambda (ht)
       (funcall (gethash 'o ht) (gethash 'l ht) (gethash 'r ht)))
   '*fold-binop-rule*) )

;list of rules
(defparameter *rules*
  (list *diff-sum-rule*
	*diff-x-rule*
	*diff-const-rule*
	*diff-product-rule*
	*diff-power-rule*
	*fold-binop-rule*
	*expt0-rule*
	*expt1-rule*
	*unity-rule*
	*times0-rule*) )
;;;
;;; Try rule on expr recursively.
;;; Return nil if rule never files,
;;; else return transformed expression.
;;;
(defun try-rule (rule expr ht)
  (cond 
   ((atom expr) nil) ; no rules match an expression that's a single atom
   ((funcall (rule-test rule) expr ht) ; see if rule matches whole expression
    (format t "~%~a fires." (rule-name rule)) ; diagnostic
    (funcall (rule-xform rule) ht) ) ; fire rule
   ;; let's recursively try the rule on (car expr) and (cdr expr)
   (t (let ((efirst (try-rule rule (car expr) ht)))
	(cond
	 (efirst (cons efirst (cdr expr))) ; (car expr) modified
	 (t (let ((erest (try-rule rule (cdr expr) ht)))
	      (cond
	       (erest (cons (car expr) erest)) ; (cdr expr) modified
	       (t nil) ) )) ) )) ) ) ; rule never fired on expr
;;
;; Try all rules on expr.
;; The transformed expr from the first rule fired is returned.
;; If no rules fire, then nil is returned.
;;
(defun try-rules (rules expr ht)
  (cond
   ((null rules) nil)
   ((null expr) nil)
   ((try-rule (car rules) expr ht))
   ((try-rules (cdr rules) expr ht)) ) )
	      

(defun reduce-expr (expr &optional (ht (make-hash-table)))
  (let ((e (try-rules *rules* expr ht)))
    (if (null e)
	expr
      (reduce-expr e ht) ) ) ) 