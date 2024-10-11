#lang info

(define version "2.0")

(define license 'ISC)

(define collection 'multi)

(define deps
  '(["threading-doc" #:version "2.0"]
    ["threading-lib" #:version "2.0"]))
(define build-deps
  '())

(define implies deps)
