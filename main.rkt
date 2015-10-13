#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide ~> ~>> and~> and~>> _
         lambda~> lambda~>> lambda~>* lambda~>>*
         lambda-and~> lambda-and~>> lambda-and~>* lambda-and~>>*
         (rename-out [lambda~> λ~>] [lambda~>> λ~>>]
                     [lambda~>* λ~>*] [lambda~>>* λ~>>*]
                     [lambda-and~> λ-and~>] [lambda-and~>> λ-and~>>]
                     [lambda-and~>* λ-and~>*] [lambda-and~>>* λ-and~>>*]))

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
      (define call (syntax->list #'cl.call))
      (define-values (pre post)
        (split-at call (add1 (or (attribute cl.insertion-point) 0))))
      (with-syntax ([(pre ...) pre]
                    [(post ...) post])
        #'(~> (pre ... ex post ...) remaining ...))])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      (define call (syntax->list #'cl.call))
      (define-values (pre post)
        (split-at call (add1 (or (attribute cl.insertion-point)
                                 (sub1 (length call))))))
      (with-syntax ([(pre ...) pre]
                    [(post ...) post])
        #'(~>> (pre ... ex post ...) remaining ...))])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      (define call (syntax->list #'cl.call))
      (define-values (pre post)
        (split-at call (add1 (or (attribute cl.insertion-point) 0))))
      (with-syntax ([(pre ...) pre]
                    [(post ...) post])
        #'(let ([v (pre ... ex post ...)])
            (and v (and~> v remaining ...))))])
   (syntax-parser
     [(_ ex:expr) #'ex]
     [(_ ex:expr cl:clause remaining:clause ...)
      (define call (syntax->list #'cl.call))
      (define-values (pre post)
        (split-at call (add1 (or (attribute cl.insertion-point)
                                 (sub1 (length call))))))
      (with-syntax ([(pre ...) pre]
                    [(post ...) post])
        #'(let ([v (pre ... ex post ...)])
            (and v (and~>> v remaining ...))))])))

(define-syntax-rule (lambda~> . body)
  (lambda (x) (~> x . body)))

(define-syntax-rule (lambda~>> . body)
  (lambda (x) (~>> x . body)))

(define-syntax-rule (lambda~>* . body)
  (lambda x (~> x . body)))

(define-syntax-rule (lambda~>>* . body)
  (lambda x (~>> x . body)))

(define-syntax-rule (lambda-and~> . body)
  (lambda (x) (and~> x . body)))

(define-syntax-rule (lambda-and~>> . body)
  (lambda (x) (and~>> x . body)))

(define-syntax-rule (lambda-and~>* . body)
  (lambda x (and~> x . body)))

(define-syntax-rule (lambda-and~>>* . body)
  (lambda x (and~>> x . body)))

(module+ test
  (require rackunit)

  (test-case
   "~> / ~>>"
   
   (check-equal? (~> 'x) 'x)
   (check-equal? (~>> 'x) 'x)
   
   (check-equal? (~> 3 add1 (- 2)) 2)
   (check-equal? (~>> 3 add1 (- 2)) -2)

   (check-equal? (~> 3 add1 (- 2 _)) -2)
   (check-equal? (~>> 3 add1 (- _ 2)) 2))

  (test-case
   "and~> / and~>>"
   
   (check-equal? (and~> 'x) 'x)
   (check-equal? (and~>> 'x) 'x)

   (check-equal? (and~> '(1 3 5) (findf odd? _) add1) 2)
   (check-equal? (and~>> '(1 3 5) (findf odd?) add1) 2)

   (check-equal? (and~> '(1 3 5) (findf even? _) add1) #f)
   (check-equal? (and~>> '(1 3 5) (findf even?) add1) #f))

  (test-case
   "Don't thread into quoted forms"

   (check-equal? (syntax->datum (expand-syntax-to-top-form #'(~> b 'a))) '('a b))
   (check-equal? (syntax->datum (expand-syntax-to-top-form #'(~>> b 'a))) '('a b))))
