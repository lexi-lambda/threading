# threading [![Build Status](https://img.shields.io/github/actions/workflow/status/lexi-lambda/threading/build.yml?branch=master)](https://github.com/lexi-lambda/threading/actions/workflows/build.yml) [![Scribble Docs](https://img.shields.io/badge/docs-built-blue)][threading-doc]

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

For more information, see [the documentation][threading-doc].

[threading-doc]: http://lexi-lambda.github.io/threading/
