#lang racket/base

(require rackunit
         threading)

(test-begin
  (define x 0)
  (define result
    (~> 10
        (+ 5)
        (tee~> - (set! x _))
        (* 2)))

  (check-equal? x -15)
  (check-equal? result 30))
