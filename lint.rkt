#lang racket/base

#|review: ignore|#

(require racket/contract
         racket/format
         racket/function
         racket/list
         racket/string
         syntax/modread
         syntax/parse)

(provide
 (struct-out problem)
 lint)

(struct problem (stx level message)
  #:transparent)

(define current-problem-list
  (make-parameter null))

(define ignore-re #rx"#\\|review: ignore\\|#")
(define (ignore? filename)
  (call-with-input-file filename
    (lambda (in)
      (regexp-match? ignore-re in))))

(define/contract (lint filename)
  (-> path-string? (listof problem?))
  (define stx (file->syntax filename))
  (when (and stx
             (not (eof-object? stx))
             (not (ignore? filename)))
    (lint-syntax! stx))

  (remove-duplicates
   (current-problem-list)))

(define (track-problem! stx message [level 'warning])
  (current-problem-list
   (cons
    (problem stx level message)
    (current-problem-list))))

(define (track-warning! stx message)
  (track-problem! stx message))

(define (track-error! stx message)
  (track-problem! stx message 'error))

(define (file->syntax filename)
  (with-handlers ([exn:fail:read?
                   (lambda (e)
                     (begin0 #f
                       (for ([loc (exn:fail:read-srclocs e)])
                         (track-error! loc (cadr (string-split (exn-message e) "read-syntax: "))))))])
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
     (track-warning! stx "missing module (#lang) declaration")
     #'e]))


;; scope ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *scope-id-seq* 0)

(define (next-scope-id!)
  (begin0 *scope-id-seq*
    (set! *scope-id-seq* (add1 *scope-id-seq*))))

(struct scope (id parent bindings)
  #:transparent)

(define (make-scope parent bindings)
  (scope (next-scope-id!) parent bindings))

(define (scope-copy s)
  (scope (scope-id s)
         (scope-parent s)
         (hash-copy (scope-bindings s))))

(define (scope-descendant? s other-s)
  (let loop ([s* (scope-parent s)])
    (cond
      [(and s* (= (scope-id s*) (scope-id other-s))) #t]
      [(and s* (scope-parent s*)) => loop]
      [else #f])))

(struct binding-info (stx usages check-usages?)
  #:transparent)

(define (make-binding-info stx [usages 0] [check-usages? #t])
  (binding-info stx usages check-usages?))

(define current-scope
  (make-parameter (make-scope #f (make-hash))))

(define current-punted-bindings  ;; name -> (listof scope)
  (make-parameter (hash)))

(define (push-scope!)
  (current-scope (make-scope (current-scope) (make-hash))))

(define (pop-scope!)
  (check-unused-bindings!)
  (current-scope (scope-parent (current-scope))))

(define (bindings-ref name)
  (let loop ([scope (current-scope)])
    (cond
      [(hash-ref (scope-bindings scope) name #f)]
      [(scope-parent scope) => loop]
      [else #f])))

(define (name-bound? name)
  (and (bindings-ref name) #t))

(define (underscores? name)
  (regexp-match-exact? #rx"_+" (cond
                                 [(symbol? name) (symbol->string name)]
                                 [else name])))

(define (name-bound-in-current-scope? name)
  (hash-has-key? (scope-bindings (current-scope)) name))

(define (track-binding! stx [fmt "~a"]
                        #:check-usages? [check-usages? #t])
  (define name (format-binding fmt stx))
  (define usages
    (cond
      [(binding-punted? name)
       (unpunt-binding! name)
       1]

      [else 0]))

  (hash-set! (scope-bindings (current-scope)) name (make-binding-info stx usages check-usages?)))

(define (punt-binding! name)
  (current-punted-bindings
   (hash-update (current-punted-bindings) name (curry cons (current-scope)) null)))

(define (unpunt-binding! name)
  (current-punted-bindings
   (hash-update (current-punted-bindings) name (curry remove (current-scope)) null)))

(define (binding-punted? name)
  (cond
    [(hash-ref (current-punted-bindings) name #f)
     => (lambda (scopes)
          (for/first ([scope (in-list scopes)]
                      #:when (scope-descendant? scope (current-scope)))
            #t))]

    [else #f]))

(define (track-binding-usage! name)
  (let loop ([scope (current-scope)])
    (define bindings (scope-bindings scope))
    (cond
      [(hash-has-key? bindings name)
       (hash-update! bindings name
                     (lambda (info)
                       (struct-copy binding-info info [usages (add1 (binding-info-usages info))])))]

      [(scope-parent scope) => loop]
      [else (punt-binding! name)])))

(define (check-unused-bindings!)
  (for ([(name binding) (in-hash (scope-bindings (current-scope)))])
    (when (and (binding-info-check-usages? binding)
               (= (binding-info-usages binding) 0)
               (not (binding-provided? name))
               (not (underscores? name)))
      (track-warning! (binding-info-stx binding) (format "identifier '~a' is never used" name)))))


;; undo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct savepoint (scope punts problems)
  #:transparent)

(define current-savepoint
  (make-parameter #f))

(define (save!)
  (current-savepoint
   (savepoint (scope-copy (current-scope))
              (current-punted-bindings)
              (current-problem-list))))

(define (undo!)
  (current-scope (savepoint-scope (current-savepoint)))
  (current-punted-bindings (savepoint-punts (current-savepoint)))
  (current-problem-list (savepoint-problems (current-savepoint))))


;; provide ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define provided-bindings
  (make-parameter null))

(define (track-provided! name)
  (provided-bindings (cons name (provided-bindings))))

(define (check-provided-bindings!)
  (for ([binding:stx (provided-bindings)])
    (define binding:id (syntax->datum binding:stx))
    (unless (name-bound? binding:id)
      (track-error! binding:stx (~a "identifier '" binding:id "' provided but not defined")))))

(define (binding-provided? name)
  (for/first ([binding:stx (provided-bindings)]
              #:when (eq? name (syntax->datum binding:stx)))
    #t))


;; rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-binding fmt . args)
  (define args:strs
    (for/list ([arg (in-list args)])
      (string-replace
       (cond
         [(symbol? arg) (symbol->string arg)]
         [(syntax? arg) (symbol->string (syntax->datum arg))]
         [else arg])
       "~"
       "~~")))

  (string->symbol (apply format fmt args:strs)))

(define-syntax-class application-expression
  (pattern (e0:expression ~! e:expression ...)))

(define-syntax-class identifier-expression
  (pattern id:id
           #:do [(track-binding-usage! (format-binding "~a" #'id))]))

(define-syntax-class lambda-expression
  #:datum-literals (lambda)
  (pattern (lambda args:define-identifier/new-scope
             e0:expression ...+
             (~do (pop-scope!))))

  (pattern (lambda
             (~do (push-scope!))
             (~or
              (~and
               (~do (save!))
               ((~seq (~optional k:keyword) arg:function-argument) ...))
              (~and
               (~do (undo!))
               ((~seq (~optional k:keyword) arg:function-argument) ... . vararg:function-argument)))
             e1:expression ...+
             (~do (pop-scope!)))))

(define-syntax-class cond-expression
  #:datum-literals (=> cond)
  (pattern (cond
             ~!
             [c:expression
              (~optional =>)
              (~do (push-scope!))
              e:expression ...+
              (~do (pop-scope!))] ...)
           #:do [(unless (eq? (last (syntax->datum #'(c ...))) 'else)
                   (track-warning! this-syntax "this cond expression does not have an else clause"))]))

(define-syntax-class if-expression
  #:datum-literals (begin if let)
  (pattern (~or (if cond:expression
                    ((~or begin let) e:expression ...+)
                    e-else:expression)
                (if cond:expression
                    e-then:expression
                    ((~or begin let) e:expression ...+)))
           #:do [(track-warning! this-syntax "use a cond expression instead of nesting begin or let inside an if")])

  (pattern (if cond:expression
               e-then:expression)
           #:do [(track-error! this-syntax "if expressions must contain one expression for the then-branch and another for the else-branch")]))

(define (define-identifier! stx)
  (cond
    [(name-bound-in-current-scope? (syntax->datum stx))
     (track-error! stx (~a "identifier '" (syntax->datum stx) "' is already defined"))]

    [(name-bound? (syntax->datum stx))
     (unless (underscores? (syntax->datum stx))
       (track-warning! stx (~a "identifier '" (syntax->datum stx) "' shadows an earlier binding")))

     (track-binding! stx)]

    [else
     (track-binding! stx)]))

(define-syntax-class define-identifier
  (pattern id:id
           #:do [(define-identifier! #'id)]))

(define-syntax-class define-identifier/new-scope
  (pattern id:id
           #:do [(push-scope!)
                 (define-identifier! #'id)]))

(define-syntax-class define-let-identifier
  (pattern [id:id e:expression]
           #:do [(unless (eq? (syntax-property this-syntax 'paren-shape) #\[)
                   (track-warning! this-syntax "bindings within a let should be surrounded by square brackets"))

                 (when (name-bound-in-current-scope? (syntax->datum #'id))
                   (track-error! #'id (~a "identifier '" (syntax->datum #'id) "' is already defined")))

                 (track-binding! #'id)]))

(define-syntax-class let-expression
  #:datum-literals (let)
  (pattern (let
             ~!
             (~do (push-scope!))
             (~optional proc-id:define-identifier)
             (id:define-let-identifier ...)
             (~do (push-scope!))
             body:expression ...
             (~do (pop-scope!)
                  (pop-scope!)))
           #:do [(when (null? (syntax-e #'(body ...)))
                   (track-error! this-syntax "let forms must contain at least one body expression"))]))

(define-syntax-class function-argument
  #:commit
  (pattern arg:define-identifier)
  (pattern [arg:define-identifier default:expression]))

(define-syntax-class function-header
  #:commit
  (pattern fun:define-identifier
           #:attr name #''fun
           #:attr depth 0)

  (pattern (~and
            (~or
             (~and
              (~do (save!))
              (fun:function-header (~do (push-scope!))  ;; must be popped by the user according to depth
                                   (~seq (~optional k:keyword) arg:function-argument) ...))
             (~and
              (~do (undo!))
              (fun:function-header (~do (push-scope!))  ;; must be popped by the user according to depth
                                   (~seq (~optional k:keyword) arg:function-argument) ...
                                   . vararg:define-identifier))))
           #:attr name (attribute fun.name)
           #:attr depth (add1 (attribute fun.depth))))

(define-syntax-class define-like
  #:datum-literals (define define-syntax define/contract define/contract/provide define/provide)
  (pattern (~or define define-syntax define/contract define/contract/provide define/provide)))

;; TODO:
;;  * define-logger
(define-syntax-class definition
  #:datum-literals (define-system define-values)
  #:commit
  (pattern (define-values (name:id ...+)
             ~!
             (~do (push-scope!))
             e:expression ...+
             (~do (pop-scope!)))
           #:do [(for ([name (in-list (syntax->list #'(name ...)))])
                   (define-identifier! name))])

  ;; from component-lib
  (pattern (define-system ~! name:id e ...)
           #:do [(track-binding! #'name "~a-system")])

  (pattern (_:define-like
            name:define-identifier
            ~!
            e:expression ...+))

  (pattern (_:define-like
            hdr:function-header
            ~!
            (~do (push-scope!))
            e:expression ...+)
           #:do [(for ([_ (in-range (add1 (attribute hdr.depth)))])
                   (pop-scope!))]))

;; TODO:
;;  * for-label
;;  * combine-in
;;  * except-in
;;  * rename-in
(define-syntax-class require-spec
  #:datum-literals (for-syntax prefix-in)
  (pattern mod:id
           #:with t 'absolute
           #:with s (symbol->string (syntax-e #'mod)))
  (pattern mod:string
           #:with t 'relative
           #:with s #'mod)
  ;; TODO: dive into these.
  (pattern (for-syntax e ...)
           #:with t 'syntax
           #:with s "")
  (pattern (prefix-in prefix:id mod:id)
           #:with t 'absolute
           #:with s (symbol->string (syntax-e #'mod)))
  (pattern (prefix-in prefix:id mod:string)
           #:with t 'relative
           #:with s #'mod))

(define-syntax-class require-statement
  #:datum-literals (require)
  (pattern (require e:require-spec ...+)
           #:do [(for ([m1 (in-list (syntax->datum #'(e.s ...)))]
                       [s1 (in-list (syntax-e #'(e ...)))]
                       [t1 (in-list (syntax->datum #'(e.t ...)))]
                       [m2 (in-list (drop (syntax->datum #'(e.s ...)) 1))]
                       [t2 (in-list (drop (syntax->datum #'(e.t ...)) 1))]
                       [s2 (in-list (drop (syntax-e #'(e ...)) 1))])
                   (cond
                     [(and (eq? t2 'syntax)
                           (not (eq? t1 t2)))
                      (track-warning! s2 (format "require (for-syntax ...) should come before all others"))]

                     [(and (eq? t1 'relative)
                           (eq? t2 'absolute))
                      (track-warning! s1 (format "require ~.s should come after ~.s" m1 m2))]

                     [(and (eq? t1 t2)
                           (string<? m2 m1))
                      (track-warning! s2 (format "require ~.s should come before ~.s" m2 m1))]))]))

(define-syntax-class provide-renamed-id
  (pattern id:id)
  (pattern e
           #:do [(track-error! #'e "not an identifier")]))

(define-syntax-class provide-spec
  #:datum-literals (contract-out rename-out struct struct-out)
  (pattern id:id
           #:do [(track-provided! #'id)])

  (pattern (contract-out
            ~!
            (~do (push-scope!)) ;; push a scope here so bindings are punted on
            [(~optional struct) name:id e:expression] ...)
           #:do [(for-each track-provided! (syntax-e #'(name ...)))
                 (pop-scope!)])

  (pattern (rename-out ~! [to-rename-id:id renamed-id:provide-renamed-id] ...)
           #:do [(for-each track-provided! (syntax-e #'(to-rename-id ...)))])

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

(define-syntax-class struct-field-spec
  (pattern name:id
           #:with mutable? #f)
  (pattern [name:id (~alt (~and #:mutable field-mutable) e) ...]
           #:with mutable? #'(~? (field-mutable ...) #f)))

(define-syntax-class struct++-field-spec
  (pattern (name:id))
  (pattern (name:id c ...+))
  (pattern ([name:id e:expression] c ...+)))

(define-syntax-class struct-definition
  #:datum-literals (serializable-struct serializable-struct/versions struct struct++)
  (pattern ((~or serializable-struct
                 serializable-struct/versions
                 struct)
             ~!
             name:id
             (~optional super-id:identifier-expression)
             (~optional version:number)
             (field:struct-field-spec ...)
             (~alt (~and #:mutable struct-mutable)
                   (~seq #:property P:expression PV:expression)
                   (~seq #:methods G:identifier-expression (d:definition ...))
                   e) ...)
           #:do [(track-binding! #'name)
                 (track-binding! #'name "~a?" #:check-usages? #f)
                 (define prefix (symbol->string (format-binding "~a" #'name)))
                 (define mutable? (not (null? (syntax->datum #'(struct-mutable ...)))))
                 (for-each (lambda (stx field-mutable?)
                             (track-binding! stx (string-append prefix "-~a"))
                             (when (or mutable? field-mutable?)
                               (track-binding! stx (string-append "set-" prefix "-~a!"))))
                           (syntax-e #'(field.name ...))
                           (map (compose1 not not syntax-e) (syntax-e #'(field.mutable? ...))))])

  (pattern (struct++
             ~!
             name:id
             (~optional super-id:identifier-expression)
             (spec:struct++-field-spec ...)
             e ...)
           #:do [(track-binding! #'name #:check-usages? #f)
                 (track-binding! #'name "~a?" #:check-usages? #f)
                 (track-binding! #'name "~a++")
                 (for-each (lambda (stx)
                             (track-binding! stx (string-append (symbol->string (format-binding "set-~a" #'name)) "-~a") #:check-usages? #f)
                             (track-binding! stx (string-append (symbol->string (format-binding "update-~a" #'name)) "-~a") #:check-usages? #f))
                           (syntax-e #'(spec.name ...)))]))

(define-syntax-class expression
  #:commit
  (pattern d:definition)
  (pattern s:struct-definition)
  (pattern l:lambda-expression)
  (pattern c:cond-expression)
  (pattern i:if-expression)
  (pattern l:let-expression)
  (pattern a:application-expression)
  (pattern I:identifier-expression)
  (pattern S:string)
  (pattern N:number)
  (pattern B:boolean)
  (pattern K:keyword)
  (pattern e))

(define-syntax-class toplevel
  #:commit
  (pattern m:module)
  (pattern r:require-statement)
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
