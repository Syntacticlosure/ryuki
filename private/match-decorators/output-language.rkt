#lang racket
(require (for-syntax syntax/parse "../base.rkt" racket/match)
         "recur-stxparam.rkt"
         "../main.rkt")

(provide @output-language)

(define-syntax (@output-language stx)
  (syntax-parse stx
    [(head lang-id) (list (match-decorator::context
                        (λ (pat tag+tagged body)
                          (with-lang-transformer #`(head lang-id #,body)))))]))