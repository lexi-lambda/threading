#lang info

(define version "1.3")

(define license 'ISC)

(define collection 'multi)

(define deps
  '(["threading-doc" #:version "1.3"]
    ["threading-lib" #:version "1.3"]))
(define build-deps
  '())

(define implies deps)
