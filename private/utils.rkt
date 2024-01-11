#lang racket
;; threading macro
(require syntax/parse/define)
(provide ~>)
(define-syntax-parser ~>
  [(_ val) #'val]
  [(_ val (thread args ...) rst ...)
   #'(~> (thread args ... val) rst ...)]
  [(_ val thread rst ...)
   #'(~> (thread val) rst ...)])

(provide (contract-out [hash-extend* (->  hash? (listof list?) hash?)])
         dbg)
(define (hash-extend* hash kvs)
  (foldr (λ (kv hash)
           (hash-set hash (first kv) (second kv))) hash kvs))

(define (dbg x)
  (displayln x)
  x)

(provide define/by-contract :)
(define-syntax-rule (define/by-contract name contract val)
  (begin (provide (contract-out [name contract]))
         (define name val)))
(define-syntax-rule (: name type)
  (provide (contract-out [name type])))



