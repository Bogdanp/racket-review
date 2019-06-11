# konmari

`konmari` lints, idents and, optionally, fixes your Racket code.  It
will, anyway, right now it doesn't do much and you shouldn't be using
it.

## Linting

`konmari` performs surface-level linting with the intent of finding
issues as quickly as it can.  As such, it does not expand the programs
it lints.  It currently detects:

* missing and invalid #lang headers
* shadowed identifiers
* redefined identifiers via `define` and `define-values`
* redefined identifiers inside `let`
* identifiers that have been provided but not defined
* `cond` expressions without an `else` clause
* usages of `begin` and `let` inside an `if`

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
