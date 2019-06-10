#lang racket/base

(require racket/contract
         racket/file
         racket/format
         racket/function
         racket/port
         racket/string
         syntax/modread
         syntax/parse)

(provide
 (struct-out problem)
 lint)

(define current-problem-list
  (make-parameter null))

(struct problem (stx level message solution)
  #:transparent)

(define/contract (lint filename)
  (-> path-string? (listof problem?))

  (define stx (file->syntax filename))
  (when (and stx (not (eof-object? stx)))
    (lint-syntax! stx))

  (current-problem-list))

(define (track-problem! stx message
                        #:level [level 'warning]
                        #:solution [solution identity])
  (current-problem-list
   (cons
    (problem stx level message solution)
    (current-problem-list))))

(define (file->syntax filename)
  (with-handlers ([exn:fail:read?
                   (lambda (e)
                     (begin0 #f
                       (for ([loc (exn:fail:read-srclocs e)])
                         (track-problem! loc (cadr (string-split (exn-message e) "read-syntax: ")) #:level 'error))))])
    (define-values (base _ __) (split-path filename))
    (parameterize ([current-load-relative-directory base]
                   [current-namespace (make-base-namespace)])
      (with-module-reading-parameterization
        (lambda ()
          (call-with-input-file filename
            (lambda (in)
              (port-count-lines! in)
              (read-syntax filename in))))))))

(define (lint-syntax! stx)
  (syntax-parse stx
    [module:module
     #'module]

    [e
     (track-problem! stx "missing module (#lang) declaration")
     #'e]))

(struct scope (parent bindings)
  #:transparent)

(define current-scope
  (make-parameter (scope #f (make-hash))))

(define (push-scope!)
  (current-scope (scope (current-scope) (make-hash))))

(define (pop-scope!)
  (when (scope-parent (current-scope))
    (current-scope (scope-parent (current-scope)))))

(define (name-bound? name)
  (let loop ([scope (current-scope)])
    (cond
      [(hash-has-key? (scope-bindings scope) name) #t]
      [(scope-parent scope) => loop]
      [else #f])))

(define (name-bound-in-current-scope? name)
  (hash-has-key? (scope-bindings (current-scope)) name))

(define (track-binding! name)
  (hash-set! (scope-bindings (current-scope)) name 'current-module))

(define-syntax-class define-identifier
  (pattern id:id
           #:do [(when (name-bound-in-current-scope? (syntax->datum #'id))
                   (track-problem! #'id (~a "identifier '" (syntax->datum #'id) "' is already defined")
                                   #:level 'error))
                 (track-binding! (syntax->datum #'id))]))

(define-syntax-class expression
  (pattern d:definition)
  (pattern e:expr))

(define-syntax-class function-argument
  (pattern arg:define-identifier
           #:do [(track-binding! (syntax->datum #'arg.id))]))

(define-syntax-class function-header
  (pattern fun:define-identifier
           #:attr name #''fun)

  (pattern (fun:function-header (~do (push-scope!))
                                arg:function-argument ...
                                (~do (pop-scope!)))
           #:attr name (attribute fun.name)))

(define-syntax-class definition
  #:datum-literals (define-values define)
  (pattern (define-values (name:define-identifier ...+)
             ~!
             (~do (push-scope!))
             e:expression ...+
             (~do (pop-scope!))))

  (pattern (define name:define-identifier
             ~!
             e:expression ...+)
           #:do [(track-binding! (syntax->datum #'name.id))])

  (pattern (define hdr:function-header
             !~
             (~do (push-scope!))
             e:expression ...+
             (~do (pop-scope!)))))

(define-syntax-class toplevel
  (pattern d:definition)
  (pattern e:expr))

(define-syntax-class module
  #:datum-literals (module module+ #%module-begin)
  (pattern ((~or module module+) name:id path:id
             (#%module-begin e:toplevel ...))))
