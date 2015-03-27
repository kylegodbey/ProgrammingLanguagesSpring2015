#lang racket
(require plai
         rackunit)

;; DATATYPES
(define-type WAEB0
  [num (n number?)]
  [id  (sym symbol?)]
  [binop (op symbol?)
         (lhs WAEB0?)
         (rhs WAEB0?)]
  [if0 (test-exp WAEB0?)
       (true-exp WAEB0?)
       (false-exp WAEB0?)]
  [bif (test-exp BEXP?)
       (true-exp WAEB0?)
       (false-exp WAEB0?)]
  [with  (var id?)
         (val WAEB0?)
         (body WAEB0?)])

(define-type BEXP
  [bool (b boolean?)]
  [binary-comparison (op symbol?)
                     (lhs WAEB0?)
                     (rhs WAEB0?)]
  [binary-conjunction (op symbol?)
                       (lhs BEXP?)
                       (rhs BEXP?)]
  [unary-bexp (op symbol?)
              (body BEXP?)])
  
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
    [(first-is? sexp 'if0)
     (if0 (parse (second sexp))
          (parse (third sexp))
          (parse (fourth sexp)))]
    [(first-is? sexp 'bif)
     (bif (parse-bexp (second sexp))
          (parse (third sexp))
          (parse (fourth sexp)))]
    [(first-is? sexp 'with)
     (with (parse (first (second sexp)))
           (parse (second (second sexp)))
           (parse (third sexp)))]
    ))

;; parse-bexp : boolean-expression -> AST
(define (parse-bexp bexp)
  (cond
    [(boolean? bexp) (bool bexp)]
    [(first-is-one-of? bexp '(< > = <= >=))
     (binary-comparison
      (first bexp)
      (parse (second bexp))
      (parse (third bexp)))]
    [(first-is-one-of? bexp '(and or xor))
     (binary-conjunction
      (first bexp)
      (parse-bexp (second bexp))
      (parse-bexp (third bexp)))]
    [(first-is? bexp 'not)
     (unary-bexp
      (first bexp)
      (parse-bexp (second bexp)))]))
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
  (type-case WAEB0 ast
    [num (n) n]
    [id  (sym)
         (let ([found (lookup sym env)])
           (if (undefined? found)
               (error 'interp "Unbound identifier: ~a" sym)
               found))]
    [binop (op lhs rhs)
           ((convert-to-function op) (interp lhs env)
               (interp rhs env))]
    [if0 (test-exp true-exp false-exp)
         (if (equal? 0 (interp test-exp env)) 
             (interp true-exp env)
             (interp false-exp env))]
    [bif (test-exp true-exp false-exp)
         (if (interp-bexp test-exp env)
             (interp true-exp env)
             (interp false-exp env))]
    [with (var val body)
          (interp 
           body
           (extend-env (binding var (interp val env))
                       env))]))


;; interp-bexp : ast env -> boolean
(define (interp-bexp ast env)
  (type-case BEXP ast
    [bool (b) b]
    [binary-comparison (op lhs rhs)
                       ((convert-to-function op) 
                        (interp lhs env)
                        (interp rhs env))]
    [binary-conjunction (op lhs rhs)
                        ((convert-to-function op)
                         (interp-bexp lhs env)
                         (interp-bexp rhs env))]
    [unary-bexp (op body)
                ((convert-to-function op) (interp-bexp body env))]))

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

;; if0 TESTS
(check-equal? (interp (parse '(if0 0 8 42)) (empty-env)) 8)
(check-equal? (interp (parse '(if0 (+ 1 0) 42 8)) (empty-env)) 8)
(check-equal? (interp (parse '(if0 (- 10 (+ 5 5)) 8 42)) (empty-env)) 8)

;; bif TESTS
(check-equal? (interp (parse '(bif (< 5 3) 42 8)) (empty-env)) 8)

(check-equal? (interp (parse '(bif (and (= 1 (- 2 1)) (> 5 3)) 8 42)) (empty-env)) 8)

(check-equal? (interp (parse '(bif (or (= 1 0) true) 8 42)) (empty-env)) 8)

(check-equal? (interp (parse '(bif (not false)) 8 42) (empty-env)) 8)

(check-equal? (interp (parse '(bif false 42 8)) (empty-env)) 8)

(check-equal? (interp (parse '(bif (or false (= 42 42)) 8 42)) (empty-env)) 8)