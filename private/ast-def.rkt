#lang racket
(require  syntax/parse
          "macro-helper.rkt"
          "utils.rkt"
          racket/syntax
          (for-template racket))

(: error-who (parameter/c symbol?))
(define error-who (make-parameter 'toplevel))

(: error-stx (parameter/c (or/c #f syntax?)))
(define error-stx (make-parameter #f))

(define ast-type/c (flat-named-contract 'ast-type
                                        (λ (x) ((or/c ast-type::listof? ast-type::contract? ast-type::self?) x))))

(provide (struct-out ast-type::listof)
         (struct-out ast-type::contract)
         (struct-out ast-type::self))

(struct/contract ast-type::listof ([t any/c]) #:transparent)
(struct/contract ast-type::contract ([con syntax?]) #:transparent)
(struct/contract ast-type::self () #:transparent)

(: ast-type::parse (-> syntax? ast-type/c))
(define ast-type::parse
  (λ (type)
    (syntax-parse type
      [((~literal listof) t) (ast-type::listof (ast-type::parse #'t))]
      [#:self (ast-type::self)]
      [_ (ast-type::contract type)])))

(: ast-type::fold (-> (-> ast-type/c any/c) ast-type/c any/c))
(define ast-type::fold  
  (λ (f type)
    (define recur (λ (x) (ast-type::fold f x)))
    (match type
      [(ast-type::listof t) (f (ast-type::listof (recur t)))]
      [(ast-type::self) (f type)]
      [(ast-type::contract con) (f type)])))

(: ast-type->contract (-> ast-type/c syntax?))
(define ast-type->contract
  (λ (type)
    (define (step type)
      (match type
        [(ast-type::listof t) #`(listof #,t)]
        [(ast-type::self) #`any/c]
        [(ast-type::contract con) con]))
    (ast-type::fold step type)))

(provide (struct-out tagged))
(struct/contract tagged ([resolved identifier?] [pred identifier?] [fields (listof ast-type/c)]) #:transparent)

(: tagged::parse (-> symbol? syntax? (values tagged? syntax?)))
(define (tagged::parse prefix stx)
  (syntax-parse stx
    [(tag:id field ...)
     (define constructor-id (format-id #'tag "~a::~a" prefix #'tag))
     (define pred-id (format-id #'tag "~a?" constructor-id))
     (define fields (map ast-type::parse (syntax->list #'(field ...))))
     
    
     (define field-names (generate-temporaries #'(field ...)))
     (define struct-fields (map (λ (n f)
                                  #`[#,n #,(ast-type->contract f)]) field-names fields))
     
     (values (tagged constructor-id pred-id fields)
             #`(struct/contract #,constructor-id #,struct-fields #:transparent))]))

(provide language-meta::constructors/c (struct-out language-meta))
(define language-meta::constructors/c (hash/c symbol? tagged?))
(struct/contract language-meta ([table
                                 (hash/dc [k symbol?] [v (k) (match k
                                                               ['constructors language-meta::constructors/c]
                                                               ['name symbol?]
                                                               ['struct-defs syntax?]
                                                               [_ any/c])])]) #:transparent)


(: language-meta::ref (-> language-meta? symbol? any/c))
(define (language-meta::ref lang-meta id)
  (match lang-meta
    [(language-meta metadata)
     (hash-ref metadata id
               (λ ()
                 (raise-argument-error 'language-meta (format "`~a` in language meta data" id) lang-meta)))]))


(: language-meta::name (-> language-meta? symbol?))
(define (language-meta::name lang-meta)
  (language-meta::ref lang-meta 'name))

(: language-meta::constructors (-> language-meta? language-meta::constructors/c))
(define (language-meta::constructors lang-meta)
  (language-meta::ref lang-meta 'constructors))

(: language-meta::resolve (-> language-meta? (or/c symbol? identifier?) tagged?))
(define (language-meta::resolve lang-meta tag)
  (define sym (if (identifier? tag) (syntax-e tag) tag))
  (hash-ref (language-meta::constructors lang-meta)
            sym
            (λ () (raise-syntax-error (error-who) (format "tag `~a` not found in language `~a`" sym
                                                          (language-meta::name lang-meta))))))
;; resolve a tag to an identifier
(: language-meta::resolve-id (-> language-meta? (or/c symbol? identifier?) identifier?))
(define (language-meta::resolve-id lang-meta tag)
  (define sym (if (identifier? tag) (syntax-e tag) tag))
  (match (language-meta::resolve lang-meta tag)
    [(tagged id _ _) id]))

(: language-meta::taggeds (-> language-meta? (listof tagged?)))
(define (language-meta::taggeds lang-meta)
  (for/list ([v (in-hash-values (language-meta::constructors lang-meta))])
    v))

(: language-meta::tags (-> language-meta? (listof symbol?)))
(define (language-meta::tags lang-meta)
  (for/list ([k (in-hash-keys (language-meta::ref lang-meta 'constructors))])
    k))
       
;; returns resolved tag identifiers
(: language-meta::tag-ids (-> language-meta? (listof symbol?)))
(define (language-meta::tag-ids lang-meta)
  (for/list ([v (language-meta::taggeds lang-meta)])
    (match v
      [(tagged id _ fields) id])))


(: check-duplicate-tags (-> (listof symbol?) any/c))
(define (check-duplicate-tags tags)
  (foldr (λ (tag set)
           (when (set-member? set tag)
             (raise-syntax-error (error-who) (format "duplicate tags: ~a" tag) (error-stx)))
           (set-add set tag))
         (seteq)
         tags))

(: check-tag-complete (-> language-meta? (listof symbol?) any/c))
(define (check-tag-complete lang-meta tags)
  (define already-have-tag-set (foldr (λ (tag set)
                                        (set-add set tag)) (seteq) tags))
  (for-each (λ (should-have-tag)
              (unless (set-member? already-have-tag-set should-have-tag)
                (raise-syntax-error (error-who)
                                    (format
                                     "exhaustiveness check failed: tag unfound: ~a" should-have-tag) (error-stx))))
            (language-meta::tags lang-meta)))
 
(: language-meta::parse (-> symbol? syntax? language-meta?))
(define (language-meta::parse name stx)
  (define clauses (syntax->list stx))
  (check-duplicate-tags (map (λ (x) (syntax-e (sCar x))) clauses))
  (define tag+tagged+def (map (λ (clause)
                                (define-values (tagged struct-def) (tagged::parse name clause))
                                (list (~> clause sCar syntax-e) tagged struct-def)) clauses))
  (define tag->tagged (foldr (λ (complex hash)
                               (hash-set hash (first complex) (second complex))) (hasheq) tag+tagged+def))
  (define struct-defs #`(begin #,@(map third tag+tagged+def)))
  (language-meta (hasheq 'constructors tag->tagged
                         'name name
                         'struct-defs (syntax-local-introduce struct-defs))))

(: language-meta::set (-> language-meta? symbol? any/c language-meta?))
(define (language-meta::set lang-meta k v)
  (match lang-meta
    [(language-meta table) (define old-data (hash-ref table k #f))
                           (when old-data
                             (raise-argument-error 'language-meta::set "a key-value not found in language-meta"
                                                   (list k v)))
                           (language-meta (hash-set table k v))]))
                                                   


(: with-lang-transformer (-> syntax? syntax?))
(define (with-lang-transformer stx)
  (syntax-parse stx
    [(head lang body1 bodies ...) (define lang-meta (syntax-local-value #'lang))
                                  (unless (language-meta? lang-meta)
                                    (raise-syntax-error 'with-lang "not a language: " #'lang))
                                  (foldr (λ (tag body)
                                           (define ident (language-meta::resolve-id lang-meta tag))
                                           (define name (datum->syntax #'head tag))
                                           #`(let-syntax ([#,name (make-rename-transformer #'#,ident)])
                                               #,body))
                                         #`(let () body1 bodies ...)
                                         (language-meta::tags lang-meta))]))