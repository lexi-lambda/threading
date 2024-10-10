#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse/pre))

(provide ~> ~>> and~> and~>> _
         lambda~> lambda~>> lambda~>* lambda~>>*
         lambda-and~> lambda-and~>> lambda-and~>* lambda-and~>>*
         (rename-out [lambda~> λ~>] [lambda~>> λ~>>]
                     [lambda~>* λ~>*] [lambda~>>* λ~>>*]
                     [lambda-and~> λ-and~>] [lambda-and~>> λ-and~>>]
                     [lambda-and~>* λ-and~>*] [lambda-and~>>* λ-and~>>*]))

(begin-for-syntax
  (define (build-insert #:pre pre-stxs
                        #:post post-stxs
                        #:context ctxt
                        #:props props)
    (λ (e-stx)
      (datum->syntax ctxt
                     (append pre-stxs (cons e-stx post-stxs))
                     ctxt
                     props)))

  (define-syntax-class (clause mode) ; mode : (or/c '~> '~>>)
    #:literals [_ quote]
    #:attributes [insert] ; insert : (-> syntax? syntax?)
    (pattern {~or {~var _ id}
                  (quote term)}
      #:attr insert (build-insert #:pre (list this-syntax)
                                  #:post '()
                                  #:context this-syntax
                                  #:props #f))
    (pattern (pre ... _ post ...)
      #:attr insert (build-insert #:pre (attribute pre)
                                  #:post (attribute post)
                                  #:context this-syntax
                                  #:props this-syntax))
    (pattern (term ...+)
      #:do [(define-values [pre post]
              (if (eq? mode '~>)
                  (split-at (attribute term) 1)
                  (values (attribute term) '())))]
      #:attr insert (build-insert #:pre pre
                                  #:post post
                                  #:context this-syntax
                                  #:props this-syntax))))

(define-syntaxes [~> ~>>]
  (let ()
    (define (make mode)
      (syntax-parser
        [(_ e:expr) #'e]
        [(head e:expr {~var c (clause mode)} rest ...)
         (datum->syntax
          this-syntax
          (list* #'head ((attribute c.insert) #'e) (attribute rest))
          this-syntax
          this-syntax)]))

    (values (make '~>) (make '~>>))))

(define-syntaxes [and~> and~>>]
  (let ()
    (define (make mode)
      (syntax-parser
        [(_ e:expr) #'e]
        [(head e:expr {~var c (clause mode)} rest ...)
         (quasisyntax/loc this-syntax
           (let ([tmp e])
             (and tmp
                  #,(datum->syntax
                     this-syntax
                     (list* #'head
                            ((attribute c.insert) #'tmp)
                            (attribute rest))
                     this-syntax
                     this-syntax))))]))

    (values (make '~>) (make '~>>))))

(define-syntaxes [lambda~> lambda~>> lambda-and~> lambda-and~>>]
  (let ()
    (define (make ~>-id)
      (syntax-parser
        [(_ . body)
         (quasisyntax/loc this-syntax
           (lambda (x)
             #,(quasisyntax/loc this-syntax
                 (#,~>-id x . body))))]))

    (values (make #'~>) (make #'~>>) (make #'and~>) (make #'and~>>))))

(define-syntaxes [lambda~>* lambda~>>* lambda-and~>* lambda-and~>>*]
  (let ()
    (define (make ~>-id)
      (syntax-parser
        [(_ . body)
         (quasisyntax/loc this-syntax
           (lambda args
             #,(quasisyntax/loc this-syntax
                 (#,~>-id args . body))))]))

    (values (make #'~>) (make #'~>>) (make #'and~>) (make #'and~>>))))
