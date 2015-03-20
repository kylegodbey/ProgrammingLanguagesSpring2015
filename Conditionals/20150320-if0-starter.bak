#lang racket
(require plai
         rackunit)

;; DATATYPES
(define-type WAE
  [num (n number?)]
  [id  (sym symbol?)]
  [binop (op symbol?)
         (lhs WAE?)
         (rhs WAE?)]
  [with  (var id?)
         (val WAE?)
         (body WAE?)])

;; PARSING HELPERS
(define (first-is? sexp sym)
  (equal? (first sexp) sym))

;; first-is-one-of? : list-of-symbols list -> boolean
;; Checks to see if the first symbol in the s-expression is a member
;; of the list. This lets us ask if we are looking at an operator
;; that is member of a set. For example:
;;
;; (first-is-one-of? sexp '(+ - * /))
;;
;; Checks to see if the first think in the sexp is one of the common
;; arithmetic operators.
(define (first-is-one-of? sexp ls)
  (member (first sexp) ls))

;; convert-to-function : symbol -> function
;; Evals a symbol in the base namespace. This lets
;; us quickly convert the symbol '+ into the function +.
;; Great fun at parties. Impress all your friends.
(define (convert-to-function sym)
  (define ns (make-base-namespace))
  (eval sym ns))

;; PARSING FUNCTIONS

;; parse : s-expression -> AST
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(first-is-one-of? sexp '(+ - * /))
     (binop (first sexp)
            (parse (second sexp))
            (parse (third sexp)))]
    [(first-is? sexp 'with)
     (with (parse (first (second sexp)))
           (parse (second (second sexp)))
           (parse (third sexp)))]
    ))

;; ENVIRONMENT HELPERS
(struct undefined ())
(struct binding (var val) #:transparent)

(define (empty-env) empty)

(define (extend-env obj env)
  (cons obj env))

(define (lookup id env)
  (cond
    [(empty? env) (undefined)]
    [(equal? id (id-sym (binding-var (first env))))
     (binding-val (first env))]
    [else 
     (lookup id (rest env))]))

;; INTERPRETING FUNCTIONS
(define (interp ast env)
  (type-case WAE ast
    [num (n) n]
    [id  (sym)
         (let ([found (lookup sym env)])
           (if (undefined? found)
               (error 'interp "Unbound identifier: ~a" sym)
               found))]
    [binop (op lhs rhs)
           ((convert-to-function op) (interp lhs env)
               (interp rhs env))]
    [with (var val body)
          (interp 
           body
           (extend-env (binding var (interp val env))
                       env))]))

;; TESTS
;; These tests are not written using the full rackunit test suite,
;; because it seems like no one is willing to write them if I use
;; the full power of rackunit. 
;; 
;; Please extend these to test the work you do.
;; These do not cover the entirety of the interpreter.

;; PARSER TESTS
(check-equal? (parse 3) (num 3))
(check-equal? (parse '(+ 3 5)) (binop '+ (num 3) (num 5)))

;; INTERPRETER TESTS
(check-equal? (interp (parse 3) (empty-env)) 3)
(check-equal? (interp (parse '(+ 3 5)) (empty-env)) 8)
