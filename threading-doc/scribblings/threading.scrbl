#lang scribble/manual

@(require (for-label racket/base
                     racket/function
                     racket/list
                     racket/math
                     threading)
          scribble/eval)

@(define threading-eval
   (make-eval-factory '(threading
                        racket/function
                        racket/list
                        racket/math)))

@(define ooo (racketmetafont "..."))

@title{Threading Macros}

@defmodule[threading]

The @racketmodname[threading] module provides a set of macros that help flatten nested function calls.
They allow value transformations to be expressed as “pipelines” of values, similar to Unix pipes.
These are called @deftech{threading macros}, and there are a number of different variations for
different purposes, but the most basic version is @racket[~>].

@; ---------------------------------------------------------------------------------------------------

@section{Guide}

Threading macros are used to take a value and “thread” it through a series of transformations to
produce a new value. In their simplest forms, they are just convenient syntax for function
composition:

@(interaction
  #:eval (threading-eval)
  (~> 1 add1 sqrt))

The above example is equivalent to the following:

@(interaction
  #:eval (threading-eval)
  (sqrt (add1 1)))

While function composition evaluates right to left, @racket[~>] threads the value from left to right.

This on its own is not terribly useful, but the benefit becomes more obvious when confronted with a
complicated nested expression:

@(interaction
  #:eval (threading-eval)
  (- (bytes-ref (string->bytes/utf-8 (symbol->string 'abc)) 1) 2))

The above expression is hard to read, especially since the nesting causes some of the arguments to be
pushed out to the right of the initial value, making it especially hard to see which function is
receiving those arguments. Using @racket[~>], this can be converted into an orderly pipeline:

@(interaction
  #:eval (threading-eval)
  (~> 'abc
      symbol->string
      string->bytes/utf-8
      (bytes-ref 1)
      (- 2)))

Note how the data flows from top to bottom in an orderly manner. Also note how some of the clauses
provided to @racket[~>] are contained within parentheses, while others are not. When no extra
arguments are provided to a function, the parentheses may be elided.

@subsection[#:tag "how-it-works"]{How @racket[~>] works}

To understand better what is happening when @racket[~>] is used, remember that it is just a macro, and
it is actually expanding precisely to the original unthreaded example. Each step of the pipeline is
successfully nested, placing the previous expression as the first argument provided to the enclosing
function. This is more easily demonstrated with an example. To start, consider the “normalized”
version of the expression, where all clauses are wrapped in parentheses:

@(racketblock
  (~> 'abc
      (symbol->string)
      (string->bytes/utf-8)
      (bytes-ref 1)
      (- 2)))

To begin, the @racket['abc] value is threaded into the first clause in the first argument position:

@(racketblock
  (~> (symbol->string 'abc)
      (string->bytes/utf-8)
      (bytes-ref 1)
      (- 2)))

This process continues, threading the new value into the next function in the pipeline:

@(racketblock
  (~> (string->bytes/utf-8 (symbol->string 'abc))
      (bytes-ref 1)
      (- 2)))

The next step is slightly more complicated, but not by much. The next clause already has an argument,
but the expansion process is the same: the "current" value is just inserted before the provided
argument:

@(racketblock
  (~> (bytes-ref (string->bytes/utf-8 (symbol->string 'abc)) 1)
      (- 2)))

Finally, the whole expression is inserted into the last clause:

@(racketblock
  (~> (- (bytes-ref (string->bytes/utf-8 (symbol->string 'abc)) 1) 2)))

Now the expansion is effectively complete—@racket[(~> x)] just expands to @racket[x] directly with no
further transformations.

@subsection{Changing the threading position}

The above example worked because each expression needed to be provided as the first argument for each
clause. This, of course, it not always the case. For example, it is frequently the opposite order that
is desired when operating on lists:

@(interaction
  #:eval (threading-eval)
  (foldl + 0 (map sqr '(1 2 3))))

In this example, using @racket[~>] would not work because the list needs to be provided as the
@emph{final} argument to @racket[map] and @racket[foldl]. Instead of using @racket[~>], this can be
achieved using its counterpart, @racket[~>>]:

@(interaction
  #:eval (threading-eval)
  (~>> '(1 2 3)
       (map sqr)
       (foldl + 0)))

The @racket[~>>] form works exactly like @racket[~>], but expressions are threaded into the final
position instead of the first.

Of course, there are times when the threading position may be inconsistent, or perhaps it needs to be
in the middle of a function call instead of at the beginning or the end. In this case, the threading
position can be explicitly specified by marking the "hole" with the @racket[_] identifier:

@(interaction
  #:eval (threading-eval)
  (~> '(1 2 3)
      (map add1 _)
      (apply + _)
      (- 1)))

Using this "hole marker" works with both @racket[~>] and @racket[~>>].

@subsection{Threading operations that can fail}

It is a Racket convention that, when an operation fails, it returns @racket[#f]. Not all functions
use this convention—sometimes it is more apt to throw an exception—but when failure is a valid state,
@racket[#f] is a convenient placeholder since it is the only falsy value in Racket.

This is a useful convention, and it works fairly well, but it can sometimes get in the way of
threading. For example, consider the following threading expression:

@(racketblock
  (~>> lst
       (findf even?)
       (* 2)))

It finds the first even number in a list, then multiplies it by two. However, if @emph{no} even number
is in the list, then it will return @racket[#f]! This will obviously cause a problem when we attempt
to multiply by two:

@(interaction
  #:eval (threading-eval)
  (~>> '(1 3 5)
       (findf even?)
       (* 2)))

This is a problem because it means the value needs to be checked in between the call to @racket[findf]
and the multiplication. If the result of @racket[findf] is @racket[#f], the whole expression should,
ideally, be @racket[#f]. Otherwise, the result should be the number multiplied by two.

Two alternative threading macros, @racket[and~>] and @racket[and~>>], are just like their ordinary
counterparts, but they will short-circuit to @racket[#f] if any intermediate values are @racket[#f],
just like @racket[and]. By using @racket[and~>>], the above expression will work correctly:

@(interaction
  #:eval (threading-eval)
  (and~>> '(1 3 5)
          (findf even?)
          (* 2))
  (and~>> '(1 4 5)
          (findf even?)
          (* 2)))

@subsection{Creating functions that thread}

The ordinary threading operations accept a value and immediately thread it through the provided
expressions, but sometimes it's helpful to simply produce a function that represents a threading
pipeline, instead. For example, one might create a function to convert a symbol to bytes:

@(racketblock
  (lambda (x)
    (~> x
        symbol->string
        string->bytes/utf-8)))

This use-case is common enough that there is a shorthand syntax, @racket[lambda~>].

@(interaction
  #:eval (threading-eval)
  (define symbol->bytes
    (lambda~> symbol->string
              string->bytes/utf-8))
  (symbol->bytes 'abc))

Arguments can be provided to @racket[lambda~>], just like @racket[~>]:

@(interaction
  #:eval (threading-eval)
  ((lambda~> (+ 3)
             (* 2))
   5))

In addition to @racket[lambda~>], there is @racket[lambda~>>], as well as @racket[lambda-and~>] and
its counterpart. All of these forms also have shorthand aliases using @tt{λ}, such as @racket[λ~>].

Finally, each threading lambda form has an additional counterpart that accepts any number of arguments
as a list instead of just taking a single argument:

@(interaction
  #:eval (threading-eval)
  ((lambda~>>* (map add1)
               (foldl * 1))
   1 2 3))

@; ---------------------------------------------------------------------------------------------------

@section{Reference}

@(define datum-clause @racket[(quote @#,racket[_datum])])

@defform[#:literals (_ quote)
         (~> form clause ...)
         #:grammar
         ([clause bare-id
                  #,datum-clause
                  (head arg ...)
                  (pre ... _ post ...)])]{
“Threads” @racket[form] through each @racket[clause], from top to bottom, as
described in @secref["how-it-works"]. More precisely, @racket[~>] expands
according to the following rules:

@itemlist[
 #:style 'compact
 @item{@racket[(~> form)] expands to simply @racket[form].}
 @item{@racket[(~> form bare-id)] expands to @racket[(bare-id form)].}
 @item{@racket[(~> form #,datum-clause)] expands to @racket[(#,datum-clause form)].}
 @item{@racket[(~> form (head arg #,ooo))] expands to @racket[(head form arg #,ooo)]}
 @item{@racket[(~> form (pre #,ooo _ post #,ooo))] expands to @racket[(pre #,ooo form post #,ooo)].}
 @item{@racket[(~> form _clause1 _clause2 #,ooo)] expands to @racket[(~> (~> form _clause1) _clause2 #,ooo)].}]

The special treatment of @datum-clause clauses is unlikely to be useful to
programs written in @hash-lang[]@|~|@racketmodname[racket], but it may be useful
in languages that provide an alternate binding for @racket[#%app].

@(examples
  #:eval (threading-eval)
  (~> '(1 2 3)
      (map add1 _)
      second
      (* 2))
  (~> "foo"
      string->bytes/utf-8
      bytes->list
      (map (curry * 2) _)
      list->bytes))

@history[#:changed "1.3" @elem{Removed the restriction that at least one
           @racket[pre] form must be present in a @racket[clause] containing
           @racket[_].}]}

@defform[#:literals (_)
         (~>> form clause ...)
         #:grammar
         ([clause bare-id
                  'datum
                  (head arg ...)
                  (pre ... _ post ...)])]{
Equivalent to @racket[~>], except that @racket[form] is inserted at the
@emph{end} of a clause of the form @racket[(head arg #,ooo)], rather than between
the @racket[head] and @racket[arg] forms.

@(examples
  #:eval (threading-eval)
  (~>> '(1 2 3)
       (map add1)
       second
       (* 2))
  (~>> "foo"
       string->bytes/utf-8
       bytes->list
       (map (curry * 2))
       list->bytes))

@history[#:changed "1.3" @elem{Removed the restriction that at least one
           @racket[pre] form must be present in a @racket[clause] containing
           @racket[_].}]}

@defform[(and~> expr clause ...)]{
Works like @racket[~>], but if any of the intermediate expressions returns @racket[#f], threading
stops, and the result of the whole expression is @racket[#f]. Like @racket[and], @racket[and~>] is
short-circuiting, so the remaining steps will not be evaluated.

@(examples
  #:eval (threading-eval)
  (and~> '(1 3 5)
         (map add1 _)
         (findf even? _))
  (and~> '(2 4 6)
         (map add1 _)
         (findf even? _)))}

@defform[(and~>> expr clause ...)]{
Combines the threading behavior of @racket[~>>] and the short-circuiting behavior of @racket[and~>].

@(examples
  #:eval (threading-eval)
  (and~>> '(1 3 5)
          (map add1)
          (findf even?))
  (and~>> '(2 4 6)
          (map add1)
          (findf even?)))}

@deftogether[(@defform[(lambda~> clause ...)]
              @defform[(λ~> clause ...)])]{
Equivalent to @racket[(λ (x) (~> x clause #,ooo))].

@(examples
  #:eval (threading-eval)
  (map (λ~> add1 (* 2)) (range 5)))}

@deftogether[(@defform[(lambda~>> clause ...)]
              @defform[(λ~>> clause ...)])]{
Like @racket[lambda~>], but uses @racket[~>>] instead of @racket[~>].}

@deftogether[(@defform[(lambda~>* clause ...)]
              @defform[(λ~>* clause ...)])]{
Equivalent to @racket[(λ args (~> args clause #,ooo))].

@(examples
  #:eval (threading-eval)
  ((λ~>* second sqr) 1 2 3))}

@deftogether[(@defform[(lambda~>>* clause ...)]
              @defform[(λ~>>* clause ...)])]{
Like @racket[lambda~>*], but uses @racket[~>>] instead of @racket[~>].}

@deftogether[(@defform[(lambda-and~> clause ...)]
              @defform[(λ-and~> clause ...)]
              @defform[(lambda-and~>> clause ...)]
              @defform[(λ-and~>> clause ...)]
              @defform[(lambda-and~>* clause ...)]
              @defform[(λ-and~>* clause ...)]
              @defform[(lambda-and~>>* clause ...)]
              @defform[(λ-and~>>* clause ...)])]{
Like @racket[lambda~>] and @racket[lambda~>*], but with the short-circuiting behavior of
@racket[and~>] and @racket[and~>>].}

