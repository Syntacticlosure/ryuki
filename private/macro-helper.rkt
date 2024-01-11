#lang racket
(require syntax/parse "utils.rkt")

(define-syntax-rule (define-accessor name binder expect)
  (define/by-contract name (-> syntax? syntax?)
    (λ (stx)
      (syntax-parse stx
        binder
        [_ (raise-argument-error 'name expect stx)]))))


(define/by-contract sMap (case-> (-> (-> syntax? syntax?) syntax? syntax?)
                                 (-> (-> syntax? syntax?) (-> syntax? syntax?)))
  (case-lambda
    [(f stx) (datum->syntax stx (map f (syntax->list stx)) stx stx)]
    [(f) (λ (stx) (sMap f stx))]))

(define/by-contract sForeach (case-> (-> (-> syntax? syntax?) syntax? syntax?)
                                     (-> (-> syntax? syntax?) (-> syntax? syntax?)))
  (case-lambda
    [(f stx) (for-each f (syntax->list stx))]
    [(f) (λ (stx) (sForeach f stx))]))

(define/by-contract sFilter (case-> (-> (-> syntax? syntax?) syntax? syntax?)
                                    (-> (-> syntax? syntax?) (-> syntax? syntax?)))
  (case-lambda
    [(f stx) (datum->syntax stx (filter f (syntax->list stx)) stx stx)]
    [(f) (λ (stx) (sFilter f stx))]))

(define/by-contract sFold (case-> (-> procedure? any/c syntax? any/c)
                                  (-> procedure? any/c (-> syntax? any/c)))
  (case-lambda
    [(cons-case nil-case stxlist) (foldr cons-case nil-case (syntax->list stxlist))]
    [(cons-case nil-case) (λ (stxlist)
                            (sFold cons-case nil-case stxlist))]))

(define/by-contract sZipWith (-> procedure? syntax? syntax? syntax?)
  (λ (f stxlist1 stxlist2)
    (datum->syntax stxlist1 (map f (syntax->list stxlist1)
                                 (syntax->list stxlist2)) stxlist1 stxlist2)))

(define/by-contract list->syntax (-> syntax? (listof syntax?) syntax?)
  (λ (ctx listofstx)
    (datum->syntax ctx listofstx ctx ctx)))

(define sPredMapContract (case-> (-> (-> syntax? boolean?) syntax? boolean?)
                                 (-> (-> syntax? boolean?) (-> syntax? boolean?))))
(define/by-contract sAndMap sPredMapContract
  (case-lambda
    [(pred) (compose1 (curry andmap pred) syntax->list)]
    [(pred stxlist) ((sAndMap pred) stxlist)]))

(define/by-contract sOrMap sPredMapContract
  (case-lambda
    [(pred) (compose1 (curry ormap pred) syntax->list)]
    [(pred stxlist) ((sOrMap pred) stxlist)]))
(define/by-contract sNull? (-> syntax? boolean?)
  (λ (stx)
    (syntax-parse stx
      [() #t]
      [_ #f])))

(define-accessor sCar [(a . b) #'a] "syntax pair")
(define-accessor sCdr [(a . b) #'b] "syntax pair")
(define-accessor sFirst [(a . b) #'a] "syntax pair")
(define-accessor sSecond [(a b . c) #'b] "syntax list that length >= 2")
(define-accessor sThird [(a b c . d) #'c] "syntax list that length >= 3")

(define/by-contract fresh-ident (-> syntax? identifier?)
  (λ (x)
    (car (generate-temporaries #`(#,x)))))
;; expand a macro unhygienicly, and be able to return anything other than syntax?
(: expand-macro (-> syntax? any/c))

(define (expand-macro stx)
  (syntax-parse stx
    [(m:id . args) ((syntax-local-value #'m) stx)]
    [m:id ((syntax-local-value #'m) stx)]
    [_ (raise-syntax-error 'expand-macro "not a correct macro form" stx)]))
     