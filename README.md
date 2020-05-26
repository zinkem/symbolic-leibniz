Symbolic Calculations and Algebra in Common Lisp

Matthew Zinke
zinkem@gmail.com
ID 11074258

Intro
-----

This is a set of lisp functions to take derivatives and simplify algebraic
expressions. 

To use:
Expressions are just as they are in lisp with prefix operators, but only 
support binary operations (ie. '(+ 2 3 x) will not work)

We have also added a derivative operator:
(d expr var) 
This operator will take the derivative of expr with respect to var when the
reduce-expr function is called. 
examples:
(d x x) returns 1
(d (x + 1) x) returns 1
(d (expt x 2) x) returns (* 2 x)

To reduce an expression call reduce-expr with your expression (in a list)
as a parameter.

The following top-level function call properly evaluates the derivate of 
(x^2 + x) to (2x + 1)

* (reduce-expr '(d (+ (expt x 2) x) x))

*DIFF-SUM-RULE* fires.
*DIFF-X-RULE* fires.
*DIFF-POWER-RULE* fires.
*DIFF-X-RULE* fires.
*EXPT1-RULE* fires.
*UNITY-RULE* fires.
(+ (* 2 X) 1)


Included in this Archive
------------------------

README -- this file
Leibniz.lisp -- File containing top level Lisp functions for reducing 
	     algebraic expressions