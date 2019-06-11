#lang racket/base

(require racket/contract
         racket/file
         racket/format
         racket/function
         racket/port
         racket/string
         racket/syntax
         syntax/modread
         syntax/parse)

(provide
 (struct-out problem)
 lint)

(struct problem (stx level message solution)
  #:transparent)

(define current-problem-list
  (make-parameter null))

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
     (check-provided-bindings!)
     #'module]

    [e
     (track-problem! stx "missing module (#lang) declaration")
     #'e]))


;; scope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct scope (parent bindings)
  #:transparent)

(struct binding-info (stx usages check-usages?)
  #:transparent)

(define (make-binding-info stx [check-usages? #t])
  (binding-info stx 0 check-usages?))

(define current-scope
  (make-parameter (scope #f (make-hash))))

(define (push-scope!)
  (current-scope (scope (current-scope) (make-hash))))

(define (pop-scope!)
  (check-unused-bindings!)
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

(define (track-binding! stx [fmt "~a"]
                        #:check-usages? [check-usages? #t])
  (hash-set! (scope-bindings (current-scope))
             (format-binding fmt stx)
             (make-binding-info stx check-usages?)))

(define (track-binding-usage! name)
  (let loop ([scope (current-scope)])
    (when scope
      (define bindings (scope-bindings scope))
      (cond
        [(hash-has-key? bindings name)
         (hash-update! bindings name (lambda (info)
                                       (struct-copy binding-info info [usages (add1 (binding-info-usages info))])))]

        [else
         (loop (scope-parent scope))]))))

(define (check-unused-bindings!)
  (for ([(name binding) (in-hash (scope-bindings (current-scope)))])
    (when (and (binding-info-check-usages? binding)
               (= (binding-info-usages binding) 0)
               (not (binding-provided? name))
               (not (regexp-match-exact? #rx"_+" (symbol->string name))))
      (track-problem! (binding-info-stx binding) (format "identifier '~a' is never used" name)))))


;; provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define provided-bindings
  (make-parameter null))

(define (track-provided! name)
  (provided-bindings (cons name (provided-bindings))))

(define (check-provided-bindings!)
  (for ([binding:stx (provided-bindings)])
    (define binding:id (syntax->datum binding:stx))
    (unless (name-bound? binding:id)
      (track-problem! binding:stx (~a "identifier '" binding:id "' provided but not defined")
                      #:level 'error))))

(define (binding-provided? name)
  (for/first ([binding:stx (provided-bindings)]
              #:when (eq? name (syntax->datum binding:stx)))
    #t))


;; rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-binding fmt . args)
  (define args:strs
    (for/list ([arg (in-list args)])
      (cond
        [(symbol? arg) (symbol->string) arg]
        [(syntax? arg) (symbol->string (syntax->datum arg))]
        [else arg])))

  (string->symbol (apply format fmt args:strs)))

(define-syntax-class application
  (pattern (e0:expression ~! e:expression ...)))

(define-syntax-class identifier
  (pattern id:id
           #:do [(track-binding-usage! (format-binding "~a" #'id))]))

(define-syntax-class define-identifier
  (pattern id:id
           #:do [(cond
                   [(name-bound-in-current-scope? (syntax->datum #'id))
                    (track-problem! #'id (~a "identifier '" (syntax->datum #'id) "' is already defined")
                                    #:level 'error)]

                   [(name-bound? (syntax->datum #'id))
                    (track-problem! #'id (~a "identifier '" (syntax->datum #'id) "' shadows an earlier binding"))])

                 (track-binding! #'id)]))

(define-syntax-class cond-expression
  #:datum-literals (cond else)
  (pattern (cond
             [c:expression e:expression ...+] ...
             [else eE:expression ...+]))
  (pattern (cond
             [c:expression e:expression ...+] ...)
           #:do [(track-problem! this-syntax "this cond expression does not have an else clause" )]))

(define-syntax-class if-expression
  #:datum-literals (begin if let)
  (pattern (~or (if cond:expression
                    ((~or begin let) e ...+)
                    e-else:expression)
                (if cond:expression
                    e-then:expression
                    ((~or begin let) e ...+)))
           #:do [(track-problem! this-syntax "use a cond expression instead of nesting begin or let inside an if")])

  (pattern (if cond:expression
               e-then:expression)
           #:do [(track-problem! this-syntax "if expressions must contain one expression for the then-branch and another for the else-branch"
                                 #:level 'error)]))

(define-syntax-class define-let-identifier
  (pattern (id:define-identifier e:expression)))

(define-syntax-class let-expression
  #:datum-literals (let)
  (pattern (let
             (~do (push-scope!))
             (~optional proc-id:define-identifier)
             (id:define-let-identifier ...)
             (~do (push-scope!))
             body:expression ...
             (~do (pop-scope!)
                  (pop-scope!)))
           #:do [(when (null? (syntax-e #'(body ...)))
                   (track-problem! this-syntax "let forms must contain at least one body expression"
                                   #:level 'error))]))

(define-syntax-class function-argument
  (pattern arg:define-identifier
           #:do [(track-binding! #'arg.id)]))

(define-syntax-class function-header
  (pattern fun:define-identifier
           #:attr name #''fun
           #:attr depth 0)

  (pattern (fun:function-header (~do (push-scope!))  ;; must be popped by the user according to depth
                                arg:function-argument ...)
           #:attr name (attribute fun.name)
           #:attr depth (add1 (attribute fun.depth))))

(define-syntax-class define-like
  (pattern id:id #:when (regexp-match? #rx"define[-/]" (symbol->string (syntax-e #'id)))))

(define-syntax-class definition
  #:datum-literals (define-values define)
  (pattern (define-values (name:define-identifier ...+)
             ~!
             (~do (push-scope!))
             e:expression ...+
             (~do (pop-scope!))))

  (pattern ((~or define _:define-like)
            name:define-identifier
            ~!
            e:expression ...+)
           #:do [(track-binding! #'name.id)])

  (pattern ((~or define _:define-like)
            hdr:function-header
            (~do (push-scope!))
            e:expression ...+
            (~do (for ([_ (in-range (add1 (attribute hdr.depth)))])
                   (pop-scope!))))))

(define-syntax-class provide-renamed-id
  (pattern id:id)
  (pattern e
           #:do [(track-problem! #'e "not an identifier"
                                 #:level 'error)]))

(define-syntax-class provide-spec
  #:datum-literals (rename-out struct-out)
  (pattern id:id
           #:do [(track-provided! #'id)])

  (pattern (rename-out ~! [to-rename-id:id renamed-id:provide-renamed-id] ...)
           #:do [(map track-provided! (syntax-e #'(to-rename-id ...)))])

  (pattern (struct-out ~! struct-id:id)
           #:do [(track-provided! #'struct-id)])

  (pattern e))

(define-syntax-class provide-statement
  #:datum-literals (provide)
  (pattern (provide e:provide-spec ...+)))

(define-syntax-class define-struct-identifier
  (pattern name:id
           #:do [(track-binding! #'name)
                 (track-binding! #'name "~a?"
                                 #:check-usages? #f)]))

(define-syntax-class struct++-spec
  (pattern (name:id))
  (pattern (name:id c))
  (pattern ([name:id e] c)))

(define-syntax-class struct-definition
  #:datum-literals (struct struct++)
  (pattern (struct ~! name:define-struct-identifier e ...))

  (pattern (struct++
             ~!
             name:define-struct-identifier
             (~optional super-id:identifier)
             (spec:struct++-spec ...)
             e ...)
           #:do [(track-binding! #'name "~a++")
                 (for-each (lambda (stx)
                             (track-binding! stx (string-append (symbol->string (format-binding "set-~a" #'name)) "-~a")
                                             #:check-usages? #f)
                             (track-binding! stx (string-append (symbol->string (format-binding "update-~a" #'name)) "-~a")
                                             #:check-usages? #f))
                           (syntax-e #'(spec.name ...)))]))

(define-syntax-class expression
  (pattern d:definition)
  (pattern s:struct-definition)
  (pattern c:cond-expression)
  (pattern i:if-expression)
  (pattern l:let-expression)
  (pattern I:identifier)
  (pattern a:application)
  (pattern e))

(define-syntax-class toplevel
  (pattern m:module)
  (pattern p:provide-statement)
  (pattern e:expression))

(define-syntax-class module
  #:datum-literals (module module+ #%module-begin)
  (pattern (module name:id path:id
             (#%module-begin ~! e:toplevel ...))
           #:do [(check-unused-bindings!)])

  (pattern (module+ ~!
             name:id
             (~do (push-scope!))
             e:toplevel ...
             (~do (pop-scope!)))))
