#lang racket/base

(require rackunit
         threading)

(check-equal? (when~> 1 #t add1 (* 2)) 4)
(check-equal? (when~> 1 #f add1 (* 2)) 1)
(check-equal? (unless~> 1 #t add1 (* 2)) 1)
(check-equal? (unless~> 1 #f add1 (* 2)) 4)

(let ()
  (define (f n)
    (~> 11
        (cond~>
          [(= n 1) add1 (* 2)]
          [(= n 2) sub1 (/ 2)]
          [else    -])))

  (check-equal? (f 1) 24)
  (check-equal? (f 2) 5)
  (check-equal? (f 3) -11))

(check-equal? (cond~> 1 [#f add1]) 1)
