#lang racket/base

(require (for-syntax racket/base
                     syntax/parse/pre)
         "base.rkt")

(provide tee~>)

(define-syntax tee~>
  (syntax-parser
    [(_ val-e:expr clause ...)
     #`(let ([tmp val-e])
         #,(syntax/loc this-syntax
             (~> tmp clause ...))
         tmp)]))
