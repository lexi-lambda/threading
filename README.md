# threading [![Build Status](https://travis-ci.org/lexi-lambda/threading.svg?branch=master)](https://travis-ci.org/lexi-lambda/threading)

This package implements threading macros for Racket.

# Installation

```sh
$ raco pkg install threading
```

# Usage

```racket
> (~> 'abc
      symbol->string
      string->bytes/utf-8
      (bytes-ref 1)
      (- 2))
96
```

For more information, see [the documentation][gh-pages].

[gh-pages]: http://lexi-lambda.github.io/threading/threading.html
