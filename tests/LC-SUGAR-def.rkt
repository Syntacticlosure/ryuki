#lang racket
(require "../private/main.rkt" "../private/match-decorators/main.rkt")

(define-AST LC-SUGAR
  (Var symbol?)
  (Lam (listof symbol?) #:self)
  (App #:self (listof                                                                                                                                                                                                                                                                                 ))
  (Begin (listof #:self))
  (Let (listof symbol?) (listof #:self) #:self))

(define (desugar exp)
  (match-lang LC-SUGAR exp #:decorators [(@recur desugar)
                                         (@output-language LC-SUGAR)
                                     (@default-transform desugar)]
              [(Begin bodies) (if (null? (cdr bodies))
                                  (last (? bodies))
                                  (let ([fresh (gensym 'var)])
                                    (App (Lam (list fresh) (desugar (Begin (cdr bodies))))
                                         (list (desugar (car bodies))))))]
              [(Let vars vals body) (App (Lam vars (? body)) (? vals))]))



