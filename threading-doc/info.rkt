#lang info

(define version "1.3")

(define license 'ISC)

(define collection 'multi)

(define deps
  '())
(define build-deps
  '("base"
    "racket-doc"
    "scribble-lib"
    ["threading-lib" #:version "1.3"]))
