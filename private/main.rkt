#lang racket
(require (for-syntax "macro-helper.rkt"
                     "ast-def.rkt"
                     "ast-match.rkt"
                     syntax/parse
                     racket/function
                     racket/provide-transform
                     "utils.rkt")
         racket/stxparam)
(provide define-AST with-lang match-lang AST-out)

(define-syntax (define-AST stx)
  (syntax-parse stx
    [(_ name . ([tag:id field ...] ...))
     #`(begin 
         (define-syntax name (parameterize ([error-who 'define-AST]
                                            [error-stx #'#,stx])
                               (language-meta::parse 'name #'([tag field ...] ...))))
         (define-AST-structs name))]))

(define-syntax (define-AST-structs stx)
  (syntax-parse stx 
    [(_ name) (syntax-local-introduce (language-meta::ref (syntax-local-value #'name) 'struct-defs))]))

(define-syntax match-lang match-lang-transformer)
(define-syntax with-lang with-lang-transformer)

(define-syntax AST-out
  (make-provide-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ name)
        (define ids (language-meta::tag-ids (syntax-local-value #'name)))
        (define struct-exports #`(combine-out #,@(sMap (λ (x) #`(struct-out #,x))
                                                       ids)))
        (expand-export struct-exports modes)]))))



