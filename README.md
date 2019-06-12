# konmari

![a screenshot of konmari being used inside Emacs](media/screenshot.png)

<p align="center">
  <strong><em>warning: experimental software ahead</em></strong>
</p>

`konmari` performs surface-level linting with the intent of finding
issues as quickly as it can.  As such, it does not expand the programs
it lints.

## Warnings

`konmari` currently emits the following warnings.

### `missing module (#lang) declaration`

``` racket
(displayln "note the lack of a #lang at the beginning of this file")
```

### `identifier * is never used`

``` racket
(define (f a b)  ;; warning is reported for a and b
  1)
```

``` racket
(define (f _ __)  ;; no warning is reported
  1)
```

### `identifier * shadows an earlier binding`

``` racket
(define a 1)

(define (f a)  ;; warning is reported for a
  a)
```

``` racket
(define _ 1)

(define (f _)  ;; no warning is reported
  #f)
```

``` racket
(define a 1)

(let ([a 2])  ;; no warning is reported -- let is exempt
  a)
```

### `this cond expression does not have an else clause`

``` racket
(cond
  [#t 1])
```

### `use a cond expression instead of nesting begin or let inside an if`

``` racket
(if #t
    (begin
      (displayln 1)
      (displayln 2))
    ...)
```

## Errors

`konmari` currently emits the following errors.

### `syntax error`

The first invalid usage of syntax in every file is reported.

### `identifier * is already defined`

``` racket
(define a 1)
(define a 2)  ;; an error is reported here
```

``` racket
(define-values (a a)
  (values 1 2))
```

``` racket
(let ([x 1]
      [x 2])  ;; an error is reported here
  (void))
```

### `identifier * provided but not defined`

``` racket
(provide a)

;; a is not defined in this module
```

This rule currently erroneously reports this error when you attempt to
provide an identifier defined in another module.

### `if expressions must contain one expression for the then-branch and another for the else branch`

``` racket
(if ok?
    #f)
```

### `let forms must contain at least one body expression`

``` racket
(let ([a 1]
      [b 2]))
```

## Emacs/flycheck support

Add the following snippet to your `init.el` to define a Flycheck
checker for konmari:

``` emacs-lisp
(flycheck-define-checker racket-konmari
  "check racket source code using konmari"
  :command ("raco" "konmari" "lint" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
  :modes racket-mode)

(add-to-list 'flycheck-checkers 'racket-konmari)
```

## Prior work

* http://planet.racket-lang.org/package-source/clements/no-brainer.plt/1/5/
* http://tmp.barzilay.org/code-ayatollah.rkt
