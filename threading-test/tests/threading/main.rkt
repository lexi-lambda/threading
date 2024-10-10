#lang racket/base

(require (prefix-in r: racket/base)
         rackunit
         threading)

(check-equal? (~> 'x) 'x)
(check-equal? (~>> 'x) 'x)
   
(check-equal? (~> 3 add1 (- 2)) 2)
(check-equal? (~>> 3 add1 (- 2)) -2)

(check-equal? (~> 3 add1 (- 2 _)) -2)
(check-equal? (~>> 3 add1 (- _ 2)) 2)

(check-equal? (~> void (_)) (void))
(check-equal? (~> add1 (_ 1)) 2)

(check-equal? (and~> 'x) 'x)
(check-equal? (and~>> 'x) 'x)

(check-equal? (and~> #f string->number) #f)
(check-equal? (and~>> #f string->number) #f)

(check-equal? (and~> '(1 3 5) (findf odd? _) add1) 2)
(check-equal? (and~>> '(1 3 5) (findf odd?) add1) 2)

(check-equal? (and~> '(1 3 5) (findf even? _) add1) #f)
(check-equal? (and~>> '(1 3 5) (findf even?) add1) #f)

(test-case
 "Don't thread into quoted forms"

 (check-equal? (syntax->datum (expand-syntax-to-top-form #'(~> b 'a))) '('a b))
 (check-equal? (syntax->datum (expand-syntax-to-top-form #'(~>> b 'a))) '('a b)))

(test-case
 "Use the #%app from the surrounding lexical context"

 (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                 (~> 1 (2) (3)))
               '(3 (2 1)))
 (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                 (~>> 1 (2) (3)))
               '(3 (2 1)))
 (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                 (and~> 1 (2) (3)))
               '(3 (2 1)))
 (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                 (and~>> 1 (2) (3)))
               '(3 (2 1)))

 (let ([->proc (λ (x) (if (symbol? x) (λ (hsh) (hash-ref hsh x)) x))]
       [h (hasheq 'x (hasheq 'y 1))])
   (let-syntax ([#%app (syntax-rules () [(_ x . rest) (r:#%app (->proc x) . rest)])])
     (check-equal? (~> h 'x 'y) 1)
     (check-equal? (~>> h 'x 'y) 1)
     (check-equal? (and~> h 'x 'y) 1)
     (check-equal? (and~>> h 'x 'y) 1)
     (check-equal? ((λ~> 'x 'y) h) 1)
     (check-equal? ((λ~>> 'x 'y) h) 1)
     (check-equal? ((λ-and~> 'x 'y) h) 1)
     (check-equal? ((λ-and~>> 'x 'y) h) 1))))
