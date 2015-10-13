#lang setup/infotab

(define collection "threading")
(define version "1.1")

(define deps
  '("base"))
(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

(define scribblings '(("scribblings/threading.scrbl")))
