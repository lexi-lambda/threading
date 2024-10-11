#lang info

(define version "2.0")

(define license 'ISC)

(define collection 'multi)

(define deps
  '())
(define build-deps
  '("base"
    "racket-doc"
    "scribble-lib"
    ["threading-lib" #:version "2.0"]))
