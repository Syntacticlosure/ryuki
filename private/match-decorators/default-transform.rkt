#lang racket
(require (for-syntax syntax/parse "../base.rkt" racket/match)
         "recur-stxparam.rkt")
(provide @default-transform)

(define-for-syntax (build-recur proc val type)
  ((ast-type::fold (λ (type)
                     (match type
                       [(ast-type::listof t) (λ (var)
                                               (define fresh (fresh-ident val))
                                               #`(map (λ (#,fresh)
                                                        #,(t fresh)) #,var))]
                       [(ast-type::self) (λ (var) (proc var))]
                       [(ast-type::contract con) (λ (var) var)])) type) val))
;; dependency: @recur
(define-syntax (@default-transform stx)
  (syntax-parse stx
    [(_ proc)
     (list (match-decorator::gen-clause
            (λ (tag+tagged*)
              (for/hasheq ([tag+tagged tag+tagged*])
                (match-define (list tag (tagged constructor _ fields)) tag+tagged)
                (define fresh-vars (map (λ (_) (fresh-ident stx)) fields))
                (define recurs (map (λ (n t) (build-recur (λ (x) #`(proc #,x)) n t)) fresh-vars fields))
                (values tag
                        #`[(#,constructor #,@fresh-vars) (#,constructor #,@recurs)])))))]))