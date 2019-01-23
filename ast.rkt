#lang racket/base

(provide (all-defined-out))


;;; Syntactic structures representing the parsed syntax
;;; ---------------------------------------------------

;;; Definition of variables
;; "let" id ":" type "=" expr
(struct Pvardef (id expr type pos)          #:transparent)

;;; Definition of functions
;; "let" ( "rec" )? id args ":" type "=" body
(struct Pfundef (rec id args body type pos) #:transparent)

;;; Identifier
;; id
(struct Pident  (id pos)                    #:transparent)

;;; Function call
;; id args
(struct Pcall   (id args pos)               #:transparent)


;;; Conditional branching
;; "if" test "then" yes "else" no
(struct Pcond   (test yes no pos)           #:transparent)


(struct Pwhile ( test exprs pos  )           #:transparent)

;;; Block
;; "begin" expr ( ";" expr )* "end"
(struct Pblock  (exprs pos)                 #:transparent)

;;; Constant values
;; value
(struct Pconst  (type value pos)            #:transparent)



;;; Syntactic structures for our internal representation (AST)
;;; ----------------------------------------------------------

;;; Definition
;; Binds <id> to <def> in the subsequent expressions.
(struct Let     (id def)        #:transparent)


;;; Variable
;; References a variable by its name <id>.
(struct Var     (id)            #:transparent)



;;; Constant value
;; Evaluates to the value of <value>.
(struct Const   (value)         #:transparent)


;;; Types
;;; -----

;;; Numbers
(define Num 'num)

;;; Strings
(define Str 'str)

;;; Booleans
(define Bool 'bool)

;;; Nil / the empty list
(define Nil 'nil)

;;; Anything
(define Any 'any)

;;; List of <t>
(struct Lst (t)        #:transparent)

;;; Function that takes <args> and returns <ret>
(struct Fun (ret args) #:transparent)
