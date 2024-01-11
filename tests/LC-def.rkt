#lang racket
(require "../private/main.rkt"
         "../private/match-decorators/main.rkt"
         (for-syntax "../private/base.rkt"))
(define-AST LC
  (Var symbol?)
  (Lam (listof symbol?) #:self)
  (App #:self (listof #:self)))
(with-lang LC (Var 'n))
(define (print-LC ast)
  (match-lang LC ast #:decorators [(@recur print-LC)]
              [(Var n) n]
              [(Lam vars body) `(lam ,vars ,(? body))]
              [(App f x) `(,(? f) ,(? x))]))
(print-LC (with-lang LC (Lam '(x y) (App (Var 'y) (list (Var 'x) (Var 'y))))))