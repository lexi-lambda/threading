#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         "base.rkt")

(provide when~> unless~> cond~>)

(define-syntax when~>
  (syntax-parser
    [(_ val-e:expr cond-e:expr clause ...)
     #`(let ([tmp val-e])
         #,(quasisyntax/loc this-syntax
             (if cond-e
                 #,(syntax/loc this-syntax
                     (~> tmp clause ...))
                 tmp)))]))

(define-syntax unless~>
  (syntax-parser
    [(_ val-e:expr cond-e:expr clause ...)
     #`(let ([tmp val-e])
         #,(quasisyntax/loc this-syntax
             (if cond-e
                 tmp
                 #,(syntax/loc this-syntax
                     (~> tmp clause ...)))))]))

(define-syntax cond~>
  (syntax-parser
    #:literals [else]
    [(_ val-e:expr
        [{~and cond-e:expr {~not else}} clause ...] ...
        {~optional [else else-clause ...]})
     (define/syntax-parse [rhs ...]
       (for/list ([clauses (in-list (attribute clause))])
         (quasisyntax/loc this-syntax
           (~> tmp #,@clauses))))
     #`(let ([tmp val-e])
         #,(quasisyntax/loc this-syntax
             (cond
               [cond-e rhs] ...
               [else #,(if (attribute else-clause)
                           (syntax/loc this-syntax
                             (~> tmp else-clause ...))
                           #'tmp)])))]))
