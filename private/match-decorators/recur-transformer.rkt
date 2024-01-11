#lang racket
(require "../base.rkt"
         syntax/parse
         (for-template racket racket/stxparam 
                       "recur-stxparam.rkt"))

(: recur-context (-> syntax? (listof match-decorator/c)))
(define (recur-context stx)
  (syntax-parse stx
    [(_ proc-stx)
     (list (match-decorator::context
      (λ (pattern tag+tagged body)
        (define fields (cdr (syntax->list pattern)))
        (match-define (tagged h _ field-types) (second tag+tagged))
        #`(syntax-parameterize ([? (recur-transformer #'proc-stx (list #,@(map (λ (x) #`#'#,x) fields))
                                                          (quote #,field-types))])
            #,body))))]))
  
(define (build-recur proc val type)
  ((ast-type::fold (λ (type)
                     (match type
                       [(ast-type::listof t) (λ (var)
                                               (define fresh (fresh-ident val))
                                               #`(map (λ (#,fresh)
                                                        #,(t fresh)) #,var))]
                       [(ast-type::self) (λ (var) (proc var))]
                       [(ast-type::contract con) (λ (var) var)])) type) val))

(define (recur-transformer proc-stx fields field-types)
  (define (field->type f)
    (call/ec (λ (break)
               (for ([field fields]
                     [field-type field-types])
                 (when (free-identifier=? f field)
                   (break field-type)))
               (raise-syntax-error 'recur "recur: not a field of Constructor" f))))
          
  (λ (stx)
    (syntax-parse stx
      [(_ name:id (~optional (~seq #:by proc) #:defaults ([proc proc-stx])))
       (build-recur (λ (v) #`(proc #,v)) #'name (field->type #'name))])))
  
