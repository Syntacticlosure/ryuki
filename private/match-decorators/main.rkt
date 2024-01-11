#lang racket
(define-syntax-rule (bounce spec ...)
  (begin 
    (require spec ...)
    (provide (all-from-out spec) ...)))

(bounce "recur.rkt" "default-transform.rkt" "output-language.rkt")