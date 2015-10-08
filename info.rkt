#lang info

(define collection "threading")
(define version "1.0.0")

(define deps
  '("base"))
(define build-deps
  '("cover"
    "cover-coveralls"
    "racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define scribblings '(("scribblings/threading.scrbl")))
