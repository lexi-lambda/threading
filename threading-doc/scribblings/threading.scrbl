#lang scribble/manual

@(begin
   (require (for-label racket/base
                       racket/format
                       racket/function
                       racket/list
                       racket/match
                       racket/math
                       racket/string
                       threading)
            scribble/example)

   (define (reftech . pre-content)
     (apply tech pre-content #:doc '(lib "scribblings/reference/reference.scrbl")))

   (define make-threading-eval (make-eval-factory '(racket/format
                                                    racket/list
                                                    racket/match
                                                    racket/math
                                                    racket/string
                                                    threading)))

   (define-syntax-rule (threading-examples body ...)
     (examples #:eval (make-threading-eval) #:once body ...))
   (define-syntax-rule (threading-interaction body ...)
     (threading-examples #:label #f body ...))

   (define ooo (racketmetafont "...")))

@title{Threading Macros}
@author{@author+email["Alexis King" "lexi.lambda@gmail.com"]}

@defmodule[threading]

@margin-note{Arguably, “pipeline macros” would be a much better name than “threading macros”, as the forms provided by this package have nothing to do with @reftech{threads}. The choice of the word “threading” comes from @hyperlink["https://clojure.org/guides/threading_macros"]{Clojure’s threading macros}, which were the original inspiration for this package. The author apologizes for not having the prudence to borrow the idea under a different name.}

This library provides a set of @italic{threading macros} that help flatten nested expressions into a more readable form. In essence, they allow a sequence of transformations to be expressed as a “pipeline”, similar to Unix pipes. Alongside the core threading operator, @racket[~>], @racketmodname[threading] provides a number of variants and helper macros useful for pipeline-oriented programming.

@local-table-of-contents[]

@; -----------------------------------------------------------------------------

@section[#:tag "introduction"]{Introduction: Reducing nesting with @racket[~>]}

When reading and writing programs, we often conceptualize the behavior of a nested expression as a series of steps. For example, we might understand the expression
@;
@(racketblock
  (- (sqrt (add1 3))))
@;
by considering how the value @racket[3] “flows through” the operations @racket[add1], @racket[sqrt], and @racket[-] in turn to yield the final result of @racket[-2]. For such a small, simple expression, this interpretation comes entirely automatically, but ease of understanding can rapidly diminish as expressions grow larger. For example, the expression
@;
@(racketblock
  (- (bytes-ref (string->bytes/utf-8 (symbol->string 'ABC)) 1) 65))
@;
is only slightly more involved, but the sequence of operations being performed can be much more challenging to immediately visually discern.

The goal of this library is to allow nested expressions like the ones above to be written in an alternate, “pipeline-like” form that more closely matches the sequential way we think about them. The core operator that makes this possible is @racket[~>], which can be used to eliminate the nesting in both of the above expressions:
@;
@(threading-interaction
  (eval:check (~> 3
                  add1
                  sqrt
                  -)
              -2)
  (eval:check (~> 'ABC
                  symbol->string
                  string->bytes/utf-8
                  (bytes-ref 1)
                  (- 65))
              1))

Each use of @racket[~>] can be read as a pipeline in which data flows from top to bottom. The first argument to @racket[~>] is the value to be threaded through the pipeline, and the remaining arguments are the pipeline’s steps. Since @racket[~>] is a macro that “threads” its argument through each step in the pipeline, we call it a @italic{threading macro}.

@subsection[#:tag "how-it-works"]{How @racket[~>] works}

The @racket[~>] operator is a syntactic shorthand for nested expressions, no more and no less. Since it is a macro, each use of @racket[~>] expands at compile-time to precisely the nested expression it represents. Expansion proceeds in steps, one for each step in the pipeline, and each expansion step adds a single layer of nesting.

To illustrate, consider the following simple example:
@;
@(racketblock
  (~> "hello"
      (string-ref 1)
      (char->integer)))
@;
In this example, the expression @racket["hello"] is threaded through two pipeline steps, @racket[(string-ref 1)] and @racket[(char->integer)]. Steps are threaded into from top to bottom, so to start, @racket["hello"] is threaded into @racket[(string-ref 1)]. When an expression is threaded into a function application, it is inserted as the first argument, so the result after a single expansion step is as follows:
@;
@(racketblock
  (~> (string-ref "hello" 1)
      (char->integer)))

Expansion continues by repeating the same process. @racket[(string-ref "hello" 1)] is threaded into @racket[(char->integer)] in the same way, inserting it as the first argument:
@;
@(racketblock
  (~> (char->integer (string-ref "hello" 1))))
@;
At this point, there are no steps left to be threaded into, so @racket[~>] just expands to the argument expression itself:
@;
@(racketblock
  (char->integer (string-ref "hello")))
@;
Now @racket[~>] has disappeared from the program completely, and expansion is complete.

Note that, in the above example, both steps in the pipeline were wrapped in parentheses. However, when a step does not actually include any extra arguments, like the @racket[(char->integer)] step above, the parentheses may be omitted, for convenience. Therefore, we could have written the example
@;
@(racketblock
  (~> "hello"
      (string-ref 1)
      char->integer))
@;
and it would have expanded in exactly the same way.

These rules are now enough to understand the expansion of both of the examples from the previous section. Let’s repeat the step-by-step illustration with the second one. The starting expression is
@;
@(racketblock
  (~> 'ABC
      symbol->string
      string->bytes/utf-8
      (bytes-ref 1)
      (- 65)))
@;
and as mentioned above, a step that is a bare identifier is just shorthand for a function application with no extra arguments. So we can start by adding parentheses to the first to steps:
@;
@(racketblock
  (~> 'ABC
      (symbol->string)
      (string->bytes/utf-8)
      (bytes-ref 1)
      (- 65)))

Now, expansion proceeds in the same way as before. First, @racket['ABC] is threaded into the first step:
@;
@(racketblock
  (~> (symbol->string 'ABC)
      (string->bytes/utf-8)
      (bytes-ref 1)
      (- 65)))
@;
The process is repeated for the second step:
@;
@(racketblock
  (~> (string->bytes/utf-8 (symbol->string 'ABC))
      (bytes-ref 1)
      (- 65)))
@;
The third step has an extra argument, but the threaded expression always comes first, so we get
@;
@;
@(racketblock
  (~> (bytes-ref (string->bytes/utf-8 (symbol->string 'ABC)) 1)
      (- 65)))
@;
and we repeat the process for the last step:
@;
@(racketblock
  (~> (- (bytes-ref (string->bytes/utf-8 (symbol->string 'ABC)) 1) 65)))

Now there are no steps left, so @racket[~>] just expands to its argument to obtain the final result:
@;
@(racketblock
  (- (bytes-ref (string->bytes/utf-8 (symbol->string 'ABC)) 1) 65))

@subsection[#:tag "threading-position"]{Controlling the threading position with @racket[_]}

In the above examples, the expression always conveniently needed to be threaded into the first argument of each step. However, that might not always be the case. For example, functions like @racket[map] and @racket[filter] take their list argument @emph{last}, so a pipeline like
@;
@(racketblock
  (~> '(-9 4 -16 25)
      (filter positive?)
      (map sqrt)))
@;
would expand to @(racketblock (map (filter '(-9 4 -16 25) positive?) sqrt)) which wouldn’t be correct. In these cases, @racket[_] can be used to explicitly mark the position to thread into:
@;
@(threading-interaction
  (~> '(-9 4 -16 25)
      (filter positive? _)
      (map sqrt _)))

Using @racket[_] makes @racket[~>] much more flexible. However, note that @racket[_] can only be used to control which argument of a pipeline step the expression is threaded into, and it cannot be used to thread an expression into a more deeply nested subexpression. For example, the following does not work as intended:
@;
@(threading-interaction
  (eval:error (~> '(1 -2 3)
                  (positive? (apply + _)))))
@;
Instead, the nesting should be broken up into separate steps so that @racket[_] isn’t nested inside a subexpression:
@;
@(threading-interaction
  (eval:check (~> '(1 -2 3)
                  (apply + _)
                  positive?)
              #t))

@subsection[#:tag "functions-that-thread"]{Functions that thread: @racket[lambda~>] and @racket[lambda~>*]}

The @racket[~>] operation is useful to immediately thread an expression through a pipeline, but sometimes it’s more useful to obtain a function that represents the pipeline itself. For example, consider the following expression:
@;
@(racketblock
  (map (lambda (x) (~> x symbol->string string-length))
       '(hello goodbye)))
@;
Here, the explicit binding of @racket[x] is really just noise. This situation is common enough to warrant a shorthand syntax, @racket[lambda~>]:
@;
@(racketblock
  (map (lambda~> symbol->string string-length)
       '(hello goodbye)))

Though @racket[lambda~>] is just an abbreviation for @racket[lambda] combined with @racket[~>], it can sometimes be useful even in situations where @racket[~>] otherwise would not be. For example, it can be used to express a lightweight form of partial application:
@;
@(threading-interaction
  (eval:check (filter (lambda~> (> 10)) '(5 10 15 20)) '(15 20)))

When used in combination with @racket[~>] and higher order functions, it’s possible to express “sub-pipelines” that operate over individual elements of a data structure produced by the outer pipeline:
@;
@(threading-interaction
  (eval:check (~> (range 20)
                  (filter (lambda~> (remainder 3)
                                    zero?)
                          _)
                  (apply + _))
              63))

Functions produced by @racket[lambda~>] always accept exactly one argument. A variant form, @racket[lambda~>*] produces a function that accepts any number of (non-keyword) arguments, and the pipeline operates on the entire argument list. Finally, both forms also have shorthand aliases, @racket[λ~>] and @racket[λ~>*].

@subsection[#:tag "threading-into-forms"]{Threading into non-function forms}

All of the examples so far have exclusively used @racket[~>] to thread expressions into function applications. When used in that way, @racket[~>] (and especially @racket[lambda~>]) can be viewed as essentially equivalent to a use of @racket[compose]. However, because @racket[~>] is a syntactic operation, it can also be used to thread into non-function forms, something @racket[compose] cannot do.

As an illustration, the following example uses @racket[~>] to thread an expression into a use of @racket[set!]:
@;
@(threading-interaction
  (define x 5)
  (~> (* x 2)
      (+ 15)
      (set! x _))
  (eval:check x 25))
@;
However, this example could still be replicated using function composition, as the use of @racket[set!] could be wrapped in a @racket[lambda]. More exotic uses of @racket[~>] cannot be replicated in that way, such as threading an expression into the body of a binding form:
@;
@(threading-interaction
  (eval:check (~> some-variable
                  (let ([some-variable 'some-value]) _))
              'some-value))

Whether or not this is actually a good idea is a matter of personal judgment. Examples like the one above are likely to be quite confusing, as they cannot be understood using the usual top-to-bottom reading of @racket[~>]. However, the ability to thread into non-function forms can be useful when those forms are written with @racket[~>] in mind, and a number of such forms are described in the following section.

@; -----------------------------------------------------------------------------

@section[#:tag "pipeline-helpers"]{Pipeline Helpers}

This section documents a handful of helper forms intended to be used as pieces of a larger @racket[~>] pipeline.

@subsection[#:tag "short-circuiting-pipelines"]{Short-circuiting pipelines: @racket[and~>]}

By convention, many Racket operations return @racket[#f] upon failure, and many others can be instructed to do so. The @racket[and~>] operator is a short-circuiting variant of @racket[~>] that takes advantage of that convention. If any step in the pipeline evaluates to @racket[#f], the remaining steps are skipped, and the entire pipeline evaluates to @racket[#f]:
@;
@(threading-interaction
  (eval:check (and~> '(1 2 3)
                     (findf even? _)
                     (* 2))
              4)
  (eval:check (and~> '(1 3 5)
                     (findf even? _)
                     (* 2))
              #f))

Although @racket[and~>] is useful standalone, it is particularly useful when combined with @racket[~>]. @racket[and~>] can be used to mark individual @emph{parts} of a pipeline short-circuiting, while the rest of the pipeline behaves as normal:
@;
@(threading-interaction
  (define (f str)
    (~> str
        string->number
        (and~> (/ 100))
        (or 0)
        (~r #:precision 2)))
  (eval:check (f "hello") "0")
  (eval:check (f "42") "0.42"))

@subsection[#:tag "conditional-pipelines"]{Conditional pipelines: @racket[when~>], @racket[unless~>], and @racket[cond~>]}

Sometimes, it can be useful to conditionally skip a portion of a pipeline. For example, consider the following function, which formats numbers as either decimals or percentages:
@;
@(threading-interaction
  (define (format-number n #:percent? percent?)
    (~> n
        (when~> percent?
          (* 100))
        (~r #:precision 2)
        (when~> percent?
          (string-append "%"))))
  (eval:check (format-number (sin 1) #:percent? #f) "0.84")
  (eval:check (format-number (sin 1) #:percent? #t) "84.15%"))

The use of @racket[when~>] allows the @racket[(* 100)] and @racket[(string-append "%")] pipeline steps to be skipped if the @racket[percent?] condition is @racket[#f].

Note that, since @racket[when~>] is intended to be used inside a @racket[~>] pipeline, it actually accepts an argument to be threaded @emph{before} the condition expression. This can appear quite strange if used standalone:
@;
@(threading-interaction
  (eval:check (when~> 'hello
                       #t
                symbol->string
                string-upcase)
              "HELLO"))

The counterpart to @racket[when~>] is @racket[unless~>], which works the same way but with its condition inverted. For even more flexibility, @racket[cond~>] can be used to switch between several pipeline alternatives.

@subsection[#:tag "pipeline-splitting"]{Pipeline splitting: @racket[tee~>]}

Suppose we have a complex pipeline and would like to inspect how it is behaving by printing the intermediate value at each pipeline step. We could insert uses of @racket[println] into the pipeline, but since @racket[println] returns @void-const, that would prevent the value from flowing to later pipeline steps. The solution is to use the @racket[tee~>] operator:
@;
@(threading-interaction
  (eval:check (~> (range 8)
                  (tee~> println)
                  (map sqr _)
                  (tee~> println)
                  (filter-not (λ~> (remainder 4) zero?) _)
                  (tee~> println)
                  (apply + _))
              84))

@racket[tee~>] is so named because it is like a pipe T-splitter: it sends its input into a separate pipeline, which is executed only for side-effects, then returns its input value so the original pipeline may continue.

@; -----------------------------------------------------------------------------

@section[#:tag "api-reference"]{API Reference}

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

The special treatment of @datum-clause clauses is unlikely to be useful to programs written in @hash-lang[]@|~|@racketmodname[racket], but it may be useful in languages that provide an alternate binding for @racket[#%app].

@(threading-examples
  (~> '(1 2 3)
      (map add1 _)
      second
      (* 2))
  (~> "foo"
      string->bytes/utf-8
      bytes->list
      (map (λ~> (* 2)) _)
      list->bytes))

@history[#:changed "1.3" @elem{Removed the restriction that at least one
           @racket[pre] form must be present in a @racket[clause] containing
           @racket[_].}]}

@deftogether[(@defform[(lambda~> clause ...)]
              @defform[(λ~> clause ...)])]{
Equivalent to @racket[(λ (x) (~> x clause #,ooo))].

@(threading-examples
  (map (λ~> add1 (* 2)) (range 5)))}

@deftogether[(@defform[(lambda~>* clause ...)]
              @defform[(λ~>* clause ...)])]{
Equivalent to @racket[(λ args (~> args clause #,ooo))].

@(threading-examples
  ((λ~>* second sqr) 1 2 3))}

@defform[(and~> expr clause ...)]{
Works like @racket[~>], but if any of the intermediate expressions returns @racket[#f], threading
stops, and the result of the whole expression is @racket[#f]. Like @racket[and], @racket[and~>] is
short-circuiting, so the remaining steps will not be evaluated.

@(threading-examples
  (and~> '(1 3 5)
         (map add1 _)
         (findf even? _))
  (and~> '(2 4 6)
         (map add1 _)
         (findf even? _)))}

@defform[(tee~> expr clause ...)]{
@margin-note{See @secref["pipeline-splitting"] for an example of using @racket[tee~>].}

Evaluates @racket[expr], then threads the result through each @racket[clause] as in @racket[~>]. The result of @racket[expr] is the result of the @racket[tee~>] expression.

@racket[tee~>] is equivalent to the following:
@;
@(racketblock
  (let ([tmp expr])
    (~> tmp clause #,ooo)
    tmp))

@history[#:added "2.0"]}

@subsection[#:tag "api-reference:conditional"]{Conditional Operators}

@defform[(when~> val-expr test-expr clause ...)]{
Evaluates @racket[val-expr] and @racket[test-expr]. If the result of @racket[test-expr] is @racket[#f], the result of the @racket[when~>] expression is the result of @racket[val-expr]. Otherwise, the result of @racket[val-expr] is threaded through each @racket[clause] as in @racket[~>].

@racket[when~>] is equivalent to the following:
@;
@(racketblock
  (let ([tmp val-expr])
    (if test-expr
        (~> tmp clause #,ooo)
        tmp)))

The @racket[when~>] form is intended to be used as part of a larger @racket[~>] pipeline; see @secref["conditional-pipelines"] for an example.

@history[#:added "2.0"]}

@defform[(unless~> val-expr test-expr clause ...)]{
Equivalent to @racket[(when~> val-expr (not test-expr) clause #,ooo)].

@history[#:added "2.0"]}

@defform[#:literals [else]
         (cond~> val-expr cond-clause ... maybe-else-clause)
         #:grammar
         ([cond-clause [test-expr clause ...]]
          [maybe-else-clause (code:line)
                             [else clause ...]])]{
Evaluates @racket[val-expr], then evaluates each @racket[test-expr], from top to bottom, until one of them produces a value other than @racket[#f]. The result of @racket[val-expr] is threaded through each @racket[clause] associated with the @racket[test-expr] that produced a non-@racket[#f] value.

If all @racket[test-expr] expressions produce @racket[#f] and an @racket[else] clause was provided, the result of @racket[val-expr] is threaded through each @racket[clause] contained in the @racket[else] clause. Otherwise, the result is the result of @racket[val-expr].

@racket[cond~>] is equivalent to the following:
@;
@(racketblock
  (let ([tmp val-expr])
    (cond
      [test-expr (~> tmp clause #,ooo)] #,ooo
      [else (~> tmp clause #,ooo)])))

Like @racket[when~>], @racket[cond~>] is intended to be used as a part of a larger @racket[~>] pipeline.

@(threading-examples
  (define (f n)
    (~> 11
        (cond~>
          [(= n 1) add1 (* 2)]
          [(= n 2) sub1 (/ 2)]
          [else    -])))

  (eval:check (f 1) 24)
  (eval:check (f 2) 5)
  (eval:check (f 3) -11))

@history[#:added "2.0"]}

@subsection[#:tag "api-reference:legacy"]{Deprecated Forms}

@defform[(~>> form clause ...)]{
@deprecated[#:what "form" @racket[~>]]

Equivalent to @racket[~>], except that @racket[form] is inserted at the @emph{end} of a clause of the form @racket[(head arg #,ooo)], rather than between the @racket[head] and @racket[arg] forms.

@history[#:changed "2.0" @elem{Deprecated in favor of using @racket[~>] with @racket[_].}]}

@defform[(and~>> expr clause ...)]{
@deprecated[#:what "form" @racket[and~>]]

Combines the threading behavior of @racket[~>>] and the short-circuiting behavior of @racket[and~>].

@history[#:changed "2.0" @elem{Deprecated in favor of using @racket[and~>] with @racket[_].}]}

@deftogether[(@defform[(lambda~>> clause ...)]
              @defform[(λ~>> clause ...)]
              @defform[(lambda~>>* clause ...)]
              @defform[(λ~>>* clause ...)]
              @defform[(lambda-and~> clause ...)]
              @defform[(λ-and~> clause ...)]
              @defform[(lambda-and~>> clause ...)]
              @defform[(λ-and~>> clause ...)]
              @defform[(lambda-and~>* clause ...)]
              @defform[(λ-and~>* clause ...)]
              @defform[(lambda-and~>>* clause ...)]
              @defform[(λ-and~>>* clause ...)])]{
@deprecated[#:what "form" @elem{@racket[lambda~>], @racket[lambda~>*], and @racket[and~>]}]

Like @racket[lambda~>], but uses @racket[~>>] instead of @racket[~>].

@history[#:changed "2.0" @elem{Deprecated in favor of combining other forms.}]}

