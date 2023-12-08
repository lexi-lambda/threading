#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse/pre))

(provide ~> ~>> and~> and~>> _
         lambda~> lambda~>> lambda~>* lambda~>>*
         lambda-and~> lambda-and~>> lambda-and~>* lambda-and~>>*
         (rename-out [lambda~> λ~>] [lambda~>> λ~>>]
                     [lambda~>* λ~>*] [lambda~>>* λ~>>*]
                     [lambda-and~> λ-and~>] [lambda-and~>> λ-and~>>]
                     [lambda-and~>* λ-and~>*] [lambda-and~>>* λ-and~>>*]))

; Adjusts the lexical context of the outermost piece of a syntax object;
; i.e. changes the context of a syntax pair but not its contents.
(define-for-syntax (adjust-outer-context ctx stx [srcloc #f])
  (datum->syntax ctx (syntax-e stx) srcloc))

(begin-for-syntax
  (define-syntax-class clause
    #:literals (_ quote)
    #:attributes (call insertion-point)
    (pattern
     id:id
     #:with call #'(id)
     #:attr insertion-point 0)
    (pattern
     'term
     #:with call #'('term)
     #:attr insertion-point 0)
    (pattern
     (head:expr pre ... _ post ...)
     #:with call #'(head pre ... post ...)
     #:attr insertion-point (length (syntax->list #'(pre ...))))
    (pattern
     (head:expr arg ...)
     #:with call #'(head arg ...)
     #:attr insertion-point #f)))

(define-syntaxes (~> ~>> and~> and~>>)
  (values
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      #:do [(define call (syntax->list #'cl.call))
            (define-values (pre post)
              (split-at call (add1 (or (attribute cl.insertion-point) 0))))]
      #:with [pre ...] pre
      #:with [post ...] post
      #:with app/ctx (adjust-outer-context this-syntax #'(pre ... ex post ...) #'cl)
      (adjust-outer-context this-syntax #'(~> app/ctx remaining ...) this-syntax)])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      #:do [(define call (syntax->list #'cl.call))
            (define-values (pre post)
              (split-at call (add1 (or (attribute cl.insertion-point)
                                       (sub1 (length call))))))]
      #:with [pre ...] pre
      #:with [post ...] post
      #:with app/ctx (adjust-outer-context this-syntax #'(pre ... ex post ...) #'cl)
      (adjust-outer-context this-syntax #'(~>> app/ctx remaining ...) this-syntax)])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      #:do [(define call (syntax->list #'cl.call))
            (define-values (pre post)
              (split-at call (add1 (or (attribute cl.insertion-point) 0))))]
      #:with [pre ...] pre
      #:with [post ...] post
      #:with app/ctx (adjust-outer-context this-syntax #'(pre ... v post ...) #'cl)
      #:with body/ctx (adjust-outer-context this-syntax #'(and~> app/ctx remaining ...) this-syntax)
      #'(let ([v ex])
          (and v body/ctx))])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      #:do [(define call (syntax->list #'cl.call))
            (define-values (pre post)
              (split-at call (add1 (or (attribute cl.insertion-point)
                                       (sub1 (length call))))))]
      #:with [pre ...] pre
      #:with [post ...] post
      #:with app/ctx (adjust-outer-context this-syntax #'(pre ... v post ...) #'cl)
      #:with body/ctx (adjust-outer-context this-syntax #'(and~>> app/ctx remaining ...) this-syntax)
      #'(let ([v ex])
          (and v body/ctx))])))

(define-syntax-rule (define-simple-macro (id . head) clause ... template)
  (define-syntax id
    (syntax-parser
      [(_ . head) clause ... #'template])))

(define-simple-macro (lambda~> . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(~> x . body) this-syntax)
  (lambda (x) arr-expr))

(define-simple-macro (lambda~>> . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(~>> x . body) this-syntax)
  (lambda (x) arr-expr))

(define-simple-macro (lambda~>* . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(~> x . body) this-syntax)
  (lambda x arr-expr))

(define-simple-macro (lambda~>>* . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(~>> x . body) this-syntax)
  (lambda x arr-expr))

(define-simple-macro (lambda-and~> . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(and~> x . body) this-syntax)
  (lambda (x) arr-expr))

(define-simple-macro (lambda-and~>> . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(and~>> x . body) this-syntax)
  (lambda (x) arr-expr))

(define-simple-macro (lambda-and~>* . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(and~> x . body) this-syntax)
  (lambda x arr-expr))

(define-simple-macro (lambda-and~>>* . body)
  #:with arr-expr (adjust-outer-context this-syntax #'(and~>> x . body) this-syntax)
  (lambda x arr-expr))
