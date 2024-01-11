#lang racket
(require (for-syntax "../base.rkt"
                     syntax/parse))

(provide @wrap-body)
(define-syntax (@wrap-body stx)
  (syntax-parse stx
    [(_ wrapper)
     (list (match-decorator::context (λ (pattern tag+tagged body)
                                 #`(wrapper #,body))))]))
 