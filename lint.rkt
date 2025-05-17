#lang racket/base

#|review: ignore|#

(require racket/contract/base
         racket/contract/region
         racket/format
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         racket/string
         racket/syntax
         setup/getinfo
         syntax/modread
         syntax/parse/pre
         (submod "ext.rkt" private)
         (only-in "ext.rkt" current-reviewer)
         "problem.rkt")

(provide
 lint)

(define ignore-all-re #rx"#\\|review: ignore\\|#")
(define ignore-line-re #rx";; (noqa|lint: ignore|review: ignore)$")
(define (lines-to-ignore filename)
  (call-with-input-file filename
    (lambda (in)
      (for/fold ([lines null])
                ([(line idx) (in-indexed (in-lines in))])
        (define all? (regexp-match? ignore-all-re line))
        #:final all?
        (cond
          [all? 'all]
          [(regexp-match? ignore-line-re line)
           (cons (add1 idx) lines)]
          [else lines])))))

(define/contract (lint filename)
  (-> path-string? (listof problem?))
  (define the-lines-to-ignore
    (lines-to-ignore filename))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (track-error! (datum->syntax #f #f (list filename 1 0 1 1))
                                   (car (string-split (exn-message e) "\n"))))])
    (define stx (file->syntax filename))
    (when (and stx
               (not (eof-object? stx))
               (not (eq? 'all the-lines-to-ignore)))
      (lint-syntax! stx)))
  (define the-problems
    (remove-duplicates
     (current-problem-list)))
  (if (pair? the-lines-to-ignore)
      (filter-not
       (λ (p)
         (define-values (_source line _col)
           (problem-loc p))
         (memv line the-lines-to-ignore))
       the-problems)
      the-problems))

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

(define (lint-submodule-syntax! stx)
  (syntax-parse stx
    #:datum-literals (module)
    [(module ~! _name _path _e:toplevel ...)
     (check-provided-bindings!)
     #'module]))

(define-runtime-module-path-index problem.rkt
  "problem.rkt")

;; Lint submodules in a fresh instance of this module, that way we don't
;; have to spend any code stacking states around. The instances share
;; problem.rkt, so the problem list itself is shared.
(define (lint-submodule-syntax!/trampoline stx)
  (define ns (current-namespace))
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (namespace-attach-module ns (module-path-index-resolve problem.rkt))
    ((dynamic-require '(submod review/lint private) 'lint-submodule-syntax!) stx)))

(module+ private
  (provide lint-submodule-syntax!))


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

(struct binding-info (stx usages check-usages? [related-stxes #:mutable])
  #:transparent)

(define (make-binding-info stx [usages 0] [check-usages? #t] [related-stxes null])
  (binding-info stx usages check-usages? related-stxes))

(define (add-related-stx! bi stx)
  (set-binding-info-related-stxes! bi (cons stx (binding-info-related-stxes bi))))

(define current-scope
  (make-parameter (make-scope #f (make-hash))))

(define current-stashed-scope
  (make-parameter #f))

(define current-punted-bindings  ;; name -> (listof scope)
  (make-parameter (hash)))

(define (push-scope!)
  (current-scope (make-scope (current-scope) (make-hash))))

(define (pop-scope! [check-bindings? #t])
  (when check-bindings?
    (check-unused-bindings!))
  (current-scope (scope-parent (current-scope))))

(define (stash-scope!)
  (current-stashed-scope (current-scope))
  (pop-scope! #f))

(define (restore-scope!)
  (unless (current-stashed-scope)
    (error 'restore-scope! "no stashed scope"))
  (current-scope (current-stashed-scope))
  (current-stashed-scope #f))

(define (bindings-ref name)
  (let loop ([scope (current-scope)])
    (cond
      [(hash-ref (scope-bindings scope) name #f)]
      [(scope-parent scope) => loop]
      [else #f])))

(define (name-bound? name)
  (and (bindings-ref name) #t))

(define (underscores? name)
  (define name:str (cond
                     [(symbol? name) (symbol->string name)]
                     [else name]))
  (string-prefix? name:str "_"))

(define (name-bound-in-current-scope? name)
  (hash-has-key? (scope-bindings (current-scope)) name))

(define (track-binding! stx [fmt "~a"]
                        #:check-usages? [check-usages? #t]
                        #:related-to [related-to-stx #f])
  (define name (format-binding fmt stx))
  (define usages
    (cond
      [(binding-punted? name)
       (unpunt-binding! name)
       1]

      [else 0]))

  (when related-to-stx
    (define related-id
      (format-binding "~a" related-to-stx))
    (cond
      [(related-provided? related-id)
       (track-provided! (datum->syntax stx name))]

      [(bindings-ref related-id)
       => (lambda (bi)
            (add-related-stx! bi (datum->syntax stx name)))]))

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

(define related-provided
  (make-parameter null))

(define all-bindings-provided?
  (make-parameter #f))

(define (track-provided! name)
  (provided-bindings (cons name (provided-bindings))))

(define (track-related-provided! name)
  (related-provided (cons name (related-provided))))

(define (track-all-provided!)
  (all-bindings-provided? #t))

(define (check-provided-bindings!)
  (for ([binding:stx (provided-bindings)])
    (define binding:id (syntax->datum binding:stx))
    (unless (name-bound? binding:id)
      (track-warning! binding:stx (~a "identifier '" binding:id "' provided but not defined")))))

(define (binding-provided? name)
  (or (all-bindings-provided?)
      (for/first ([binding:stx (provided-bindings)]
                  #:when (eq? name (syntax->datum binding:stx)))
        #t)))

(define (related-provided? name)
  (for/first ([binding:stx (related-provided)]
              #:when (eq? name (syntax->datum binding:stx)))
    #t))


;; rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (format-binding fmt . args)
  (define args:strs
    (for/list ([arg (in-list args)])
      (cond
        [(symbol? arg) (symbol->string arg)]
        [(syntax? arg) (symbol->string (syntax->datum arg))]
        [else arg])))

  (string->symbol (apply format fmt args:strs)))

(define-syntax-class contract-arrow-dom-expression
  (pattern (rator:expression ~! rand:expression ...)
           #:do [(when (syntax-property this-syntax 'paren-shape)
                   (unless (eq? (syntax-property this-syntax 'paren-shape) #\()
                     (track-warning! this-syntax "contract domain expressions should use round parens; did you mean to use ->*?")))])
  (pattern expr:expression))

(define-splicing-syntax-class contract-arrow-dom
  (pattern {~seq kwd:keyword expr:contract-arrow-dom-expression})
  (pattern expr:contract-arrow-dom-expression))

(define-syntax-class contract-arrow-mandatory-dom
  (pattern [dom:contract-arrow-dom ...]
           #:do [(unless (eq? (syntax-property this-syntax 'paren-shape) #\[)
                   (track-warning! this-syntax "mandatory domain parens should be square"))]))

(define-syntax-class contract-arrow-optional-dom
  (pattern [dom:contract-arrow-dom ...]
           #:do [(unless (eq? (syntax-property this-syntax 'paren-shape) #\[)
                   (track-warning! this-syntax "optional domain parens should be square"))] ))

(define-syntax-class contract-arrow-range
  #:datum-literals (any values)
  (pattern any)
  (pattern (values ~! e:expression ...))
  (pattern e:expression))

(define-syntax-class contract-arrow-expression
  #:datum-literals (-> ->*)
  (pattern (-> dom:contract-arrow-dom ... range-expr:contract-arrow-range))
  (pattern (-> dom:contract-arrow-dom ... {~datum ...} dom-expr:contract-arrow-dom-expression ... range-expr:contract-arrow-range))
  (pattern (->* mandatory-dom:contract-arrow-mandatory-dom
                {~optional optional-dom:contract-arrow-optional-dom}
                {~optional {~seq #:rest rest-expr:expression}}
                {~optional {~or {~seq #:pre pre-cond-expr:expression}
                                {~seq #:pre/desc pre-cond-expr:expression}}}
                range:contract-arrow-range
                {~optional {~or {~seq #:post post-cond-expr:expression}
                                {~seq #:post/desc post-cond-expr:expression}}})))

(define-syntax-class application-expression
  (pattern (e0:expression ~! e:expression ...)))

(define-syntax-class identifier-expression
  (pattern id:id
           #:do [(track-binding-usage! (format-binding "~a" #'id))
                 (when (eq? (syntax->datum #'id) 'false/c)
                   (track-warning! this-syntax "prefer #f over false/c"))]))

(define-syntax-class lambda-expression
  #:datum-literals (case-lambda lambda λ)
  (pattern ((~or lambda λ) args:define-identifier/new-scope
             e0:expression ...+
             (~do (pop-scope!))))

  (pattern ((~or lambda λ)
             (~do (push-scope!))
             (~or
              (~and
               (~do (save!))
               ((~seq (~optional k:keyword) arg:function-argument) ...))
              (~and
               (~do (undo!))
               ((~seq (~optional k:keyword) arg:function-argument) ... . vararg:function-argument)))
             e1:expression ...+
             (~do (pop-scope!))))

  (pattern (case-lambda
             [{~do (push-scope!)}
              {~or
               {~and
                {~do (save!)}
                (arg:define-identifier ...)}
               {~and
                {~do (undo!)}
                {~do (save!)}
                (arg:define-identifier ...+ . vararg:define-identifier)}
               {~and
                {~do (undo!)}
                vararg:define-identifier}}
              e2:expression ...+
              {~do (pop-scope!)}] ...)))

(define-syntax-class case-clause
  #:datum-literals (quote quasiquote else)
  (pattern else)
  (pattern ({~and {~or* quote quasiquote} qt} _)
           #:when (equal? (syntax-span #'qt) 1)
           #:do [(track-error! this-syntax "case clause must be in the form (<const> ...), not '<const>")])
  (pattern (_ ...)))

(define-syntax-class cond-expression
  #:datum-literals (=> case cond unless when)
  (pattern (case
             ~!
             ce:expression
             [:case-clause
              (~do (push-scope!))
              e:expression ...+
              (~do (pop-scope!))] ...))

  (pattern (cond
             ~!
             [c:expression
              (~optional =>)
              (~do (push-scope!))
              e:expression ...
              (~do (pop-scope!))] ...)
           #:do [(for ([clause-stx (in-list (cdr (syntax-e this-syntax)))])
                   (unless (eq? (syntax-property clause-stx 'paren-shape) #\[)
                     (track-warning! clause-stx "this cond clause should be surrounded by square brackets")))
                 (unless (eq? (last (syntax->datum #'(c ...))) 'else)
                   (track-warning! this-syntax "this cond expression does not have an else clause"))])

  (pattern ({~or unless when} uc:expression
             {~do (push-scope!)}
             ue:expression ...+
             {~do (pop-scope!)})))

(define (try-track-struct-usage! struct-id-stx)
  (cond
    ;; We don't know for sure that this is a struct
    ;; binding, but this is good enough for now.
    [(bindings-ref (format-binding "~a" struct-id-stx))
     => (lambda (bi)
          (track-binding-usage! (format-binding "~a" struct-id-stx))
          (for ([stx (in-list (binding-info-related-stxes bi))])
            (track-binding-usage! (format-binding "~a" stx))))]))

(define-syntax-class match-pattern
  #:datum-literals (_ ? and app cons else empty or not null quote quasiquote struct* unquote)
  (pattern _)
  (pattern else
           #:do [(track-error! this-syntax "use _ instead of else in the fallthrough case of a match expression")])
  (pattern {~or* null empty}
           #:do [(track-error! this-syntax "use '() for match pattern instead of null or empty")])
  (pattern (? e:expression arg:match-pattern ...))
  (pattern ({~or and or} arg:match-pattern ...))
  (pattern (not arg:match-pattern))
  (pattern (app e:expression arg:match-pattern))
  (pattern (struct* struct-id:id ([field-id:id field-pattern:match-pattern] ...))
           #:do [(try-track-struct-usage! #'struct-id)])
  (pattern (cons a:match-pattern b:match-pattern))
  (pattern (quote literal))
  (pattern (quasiquote
            {~or (unquote p:match-pattern)
                 ({~or (unquote e:match-pattern) literal} ...)}))
  (pattern (struct-id:id arg:match-pattern ...)
           #:do [(try-track-struct-usage! #'struct-id)])
  (pattern id:id #:do [(track-binding! #'id)])
  (pattern e))

(define-syntax-class match-expression
  #:datum-literals (match match-define match-lambda)
  (pattern (match-define ~!
             dpat:match-pattern
             de:expression))

  (pattern (match ~!
             e:expression
             [{~do (push-scope!)}
              pat:match-pattern
              ce:expression ...+
              {~do (pop-scope!)}] ...+))

  (pattern (match-lambda
             [{~do (push-scope!)}
              pat:match-pattern
              ce:expression ...+
              {~do (pop-scope!)}] ...+)))

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

(define (define-identifier!
          #:check-shadow? [check-shadow? #t]
          #:check-usages? [check-usages? #t]
          #:related-to [related-to-stx #f]
          stx)
  (cond
    [(name-bound-in-current-scope? (syntax->datum stx))
     (track-error! stx (~a "identifier '" (syntax->datum stx) "' is already defined"))]

    [(and check-shadow? (name-bound? (syntax->datum stx)))
     (unless (underscores? (syntax->datum stx))
       (track-warning! stx (~a "identifier '" (syntax->datum stx) "' shadows an earlier binding")))

     (track-binding!
      #:check-usages? check-usages?
      #:related-to related-to-stx
      stx)]

    [else
     (track-binding!
      #:check-usages? check-usages?
      #:related-to related-to-stx
      stx)]))

(define-syntax-class define-identifier
  (pattern id:id
           #:do [(define-identifier! #'id)]))

(define-syntax-class (define-identifier* shadow-ok?)
  (pattern id:id
           #:do [(define-identifier!
                   #:check-shadow? (not shadow-ok?)
                   #'id)]))

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

(define-syntax-class define-for-clause
  (pattern [id:id e:expression]
           #:do [(unless (eq? (syntax-property this-syntax 'paren-shape) #\[)
                   (track-error! this-syntax "'for' clauses should be surrounded by square brackets"))
                 (when (name-bound-in-current-scope? (syntax->datum #'id))
                   (track-error! #'id (~a "identifier '" (syntax->datum #'id) "' is already defined in another clause")))
                 (track-binding! #'id)])
  (pattern [(ids:id ...) e:expression]
           #:do [(unless (eq? (syntax-property this-syntax 'paren-shape) #\[)
                   (track-error! this-syntax "'for' clauses should be surrounded by square brackets"))
                 (for ([id-stx (in-list (syntax-e #'(ids ...)))])
                   (when (name-bound-in-current-scope? (syntax->datum id-stx))
                     (track-error! #'id (~a "identifier '" (syntax->datum id-stx) "' is already defined in another clause")))
                   (track-binding! id-stx))]))

(define-splicing-syntax-class for-keyword
  (pattern (~seq #:do [do-e:expression ...]))
  (pattern (~seq (~or #:when #:unless #:break #:final) e:expression)))

(define-syntax-class for-expression
  #:datum-literals (for for*
                     for/and for*/and
                     for/or for*/or
                     for/first for*/first
                     for/list for*/list
                     for/hash for*/hash
                     for/hasheq for*/hasheq
                     for/hasheqv for*/hasheqv
                     for/vector for*/vector
                     for/fold for*/fold
                     for/lists for*/lists)
  (pattern ((~or for for*
                 for/and for*/and
                 for/or for*/or
                 for/first for*/first
                 for/list for*/list
                 for/hash for*/hash
                 for/hasheq for*/hasheq
                 for/hasheqv for*/hasheqv)
            ~!
            (~do (push-scope!))
            ((~or clause:define-for-clause kwd-clause:for-keyword) ...)
            (~do (push-scope!))
            (~or (~seq (~or #:break #:final) break-e:expression) body:expression) ...
            (~do (pop-scope!)
                 (pop-scope!)))
           #:do [(when (null? (syntax-e #'(body ...)))
                   (track-error! this-syntax "for forms must contain at least one body expression"))])

  (pattern ((~or for/vector for*/vector)
            ~!
            (~optional (~seq #:length length-e:expression))
            (~do (push-scope!))
            ((~or clause:define-for-clause kwd-clause:for-keyword) ...)
            (~do (push-scope!))
            (~or (~seq (~or #:break #:final) break-e:expression) body:expression) ...
            (~do (pop-scope!)
                 (pop-scope!)))
           #:do [(when (null? (syntax-e #'(body ...)))
                   (track-error! this-syntax "for forms must contain at least one body expression"))])

  (pattern ((~or for/fold for*/fold)
            ~!
            (~do (push-scope!)
                 (push-scope!))
            (accum-id:define-for-clause ... (~optional (~seq #:result result-e:expression)))
            (~do (stash-scope!))
            ((~or clause:define-for-clause kwd-clause:for-keyword) ...)
            (~do (restore-scope!))
            (~or (~seq (~or #:break #:final) break-e:expression) body:expression) ...
            (~do (pop-scope!)
                 (pop-scope!)))
           #:do [(when (null? (syntax-e #'(body ...)))
                   (track-error! this-syntax "for forms must contain at least one body expression"))])

  (pattern ((~or for/lists for*/lists)
            ~!
            (~do (push-scope!))
            (id:define-identifier ... (~optional (~seq #:result result-e:expression)))
            (~do (push-scope!))
            ((~or clause:define-for-clause kwd-clause:for-keyword) ...)
            (~do (push-scope!))
            (~or (~seq (~or #:break #:final) break-e:expression) body:expression) ...
            (~do (pop-scope!)
                 (pop-scope!)
                 (pop-scope!)))
           #:do [(when (null? (syntax-e #'(body ...)))
                   (track-error! this-syntax "for forms must contain at least one body expression"))]))

;; Expressions that introduce new scopes, like rackunit's `test-case'.
(define-syntax-class scoping-expression-id
  #:datum-literals (delay delay/sync delay/thread test-case test-suite)
  (pattern {~or delay delay/sync delay/thread test-case test-suite}))

(define-syntax-class scoping-expression
  (pattern (id:scoping-expression-id ~!
             (~do (push-scope!))
             e:expression ...
             (~do (pop-scope!)))))

(define-syntax-class function-argument
  #:commit
  (pattern arg:define-identifier)
  (pattern [arg:id default:expression]
           ;; This pattern must call define identifier _after_ the
           ;; default expression is linted to avoid adding an identifier
           ;; to the current scope that could potentially shadow an
           ;; identifier used by the default expression. See the
           ;; "shadow-arg.rkt" test.
           #:do [(define-identifier! #'arg)]))

;; A user of this pattern must apply (attribute p.pop-scopes!) to pop
;; all the scopes it may have introduced while walking the nested
;; function headers.
(define-syntax-class (function-header shadow-ok?)
  #:commit
  (pattern {~var fun (define-identifier* shadow-ok?)}
           #:attr name #''fun
           #:attr depth 0
           #:attr pop-scopes! void)

  (pattern {~and
            {~or
             {~and
              {~do (save!)}
              ({~var fun (function-header shadow-ok?)}
               {~do (push-scope!)}  ;; must be popped by the user according to depth
               {~seq {~optional k:keyword} arg:function-argument} ...)}
             {~and
              {~do (undo!)}
              ({~var fun (function-header shadow-ok?)}
               {~do (push-scope!)}  ;; must be popped by the user according to depth
               {~seq {~optional k:keyword} arg:function-argument} ...
               . vararg:define-identifier)}}}
           #:attr name (attribute fun.name)
           #:attr depth (add1 (attribute fun.depth))
           #:attr pop-scopes! (lambda ()
                                (for ([_ (in-range (attribute depth))])
                                  (pop-scope!)))))

(define-syntax-class class-define
  (pattern {~or {~datum define/public}
                {~datum define/pubment}
                {~datum define/public-final}
                {~datum define/override}
                {~datum define/overment}
                {~datum define/override-final}
                {~datum define/augment}
                {~datum define/augride}
                {~datum define/augment-final}
                {~datum define/private}}))

(define-syntax-class define-like
  (pattern id:id #:when (string-prefix? (symbol->string (syntax->datum #'id)) "define")))

(define-syntax-class definition
  #:datum-literals (define-generics define-logger define-syntax define-syntax-class define-syntax-rule define-syntax-parser define-system define-values define/match)
  #:commit
  (pattern (define-syntax name:id ~! stx-e)
           #:do [(track-binding! #'name)])

  (pattern (define-syntax ~! (name:id stx-arg)
             stx-e ...+)
           #:do [(track-binding! #'name)])

  (pattern (define-syntax-class ~! name:id . _rest)
           #:do [(track-binding! #'name #:check-usages? #f)])

  (pattern (define-syntax-rule ~! (name:id arg ...)
             stx-e ...+)
           #:do [(track-binding! #'name)])

  (pattern (define-syntax-parser ~! name:id stx-e ...+)
           #:do [(track-binding! #'name)])

  (pattern (define-values (name:id ...+)
             ~!
             (~do (push-scope!))
             e:expression ...+
             (~do (pop-scope!)))
           #:do [(for ([name (in-list (syntax->list #'(name ...)))])
                   (define-identifier! name))])

  ;; from racket/generic
  (pattern (define-generics ~! name:id
             (fn-name:id arg:id ...) ...
             e ...)
           #:do [(define gen-id (format-id #'name "gen:~a" #'name #:source #'name))
                 (define-identifier! gen-id)
                 (define-identifier!
                   #:related-to gen-id
                   (format-id #'name "~a?" #'name #:source #'name) )
                 (for ([fn (in-list (syntax->list #'((fn-name arg ...) ...)))])
                   (define parts (syntax->list fn))
                   (define fn-name (car parts))
                   (define fn-args (cdr parts))
                   (define-identifier! #:related-to gen-id fn-name)

                   (define generic-found?
                     (for/first ([fn-arg (in-list fn-args)]
                                 #:when (eq? (syntax->datum fn-arg)
                                             (syntax->datum #'name))) #t))

                   (unless generic-found?
                     (track-warning! fn-name (~a "generic identifier '" (syntax->datum #'name) "' not bound in definition"))))])

  (pattern (define-logger ~! name:id e ...)
           #:do [(define-identifier! #:check-usages? #f (format-id #'name "~a-logger" #'name #:source #'name))
                 (define-identifier! #:check-usages? #f (format-id #'name "log-~a-fatal" #'name #:source #'name))
                 (define-identifier! #:check-usages? #f (format-id #'name "log-~a-error" #'name #:source #'name))
                 (define-identifier! #:check-usages? #f (format-id #'name "log-~a-warning" #'name #:source #'name))
                 (define-identifier! #:check-usages? #f (format-id #'name "log-~a-info" #'name #:source #'name))
                 (define-identifier! #:check-usages? #f (format-id #'name "log-~a-debug" #'name #:source #'name))])

  ;; from component-lib
  (pattern (define-system ~! name:id e ...)
           #:do [(track-binding! #'name "~a-system")])

  (pattern (define/match
             {~var hdr (function-header #f)}
             ~!
             {~do (push-scope!)}
             c:match-pattern ...+)
           #:do [(pop-scope!)
                 ((attribute hdr.pop-scopes!))])

  (pattern (_:class-define name:id ~! e:expression))
  (pattern (_:class-define
            (name:id
             ~!
             {~do (push-scope!)}
             {~seq {~optional kwd:keyword} arg:function-argument} ...)
            {~do (push-scope!)}
            e:expression ...+)
           #:do [(pop-scope!)
                 (pop-scope!)])

  (pattern (define-id:define-like
            name:define-identifier
            ~!
            e:expression ...+)
           #:do [(track-binding-usage! (format-binding "~a" #'define-id))])

  (pattern (define-id:define-like
            {~var hdr (function-header #f)}
            ~!
            {~do (push-scope!)}
            e:expression ...+)
           #:do [(track-binding-usage! (format-binding "~a" #'define-id))
                 (pop-scope!)
                 ((attribute hdr.pop-scopes!))]))

(define (check-requires-sorted stxs mod-stxs type-stxs)
  (for ([m1 (in-list (syntax->datum mod-stxs))]
        [t1 (in-list (syntax->datum type-stxs))]
        [s1 (in-list (syntax-e stxs))]
        [m2 (in-list (cdr (syntax->datum mod-stxs)))]
        [t2 (in-list (cdr (syntax->datum type-stxs)))]
        [s2 (in-list (cdr (syntax-e stxs)))])
    (cond
      [(and (eq? t2 'syntax)
            (not (eq? t1 t2)))
       (track-warning! s2 (format "require (for-syntax ...) should come before all others"))]

      [(and (eq? t1 'relative)
            (eq? t2 'absolute))
       (track-warning! s1 (format "require ~.s should come after ~.s" m1 m2))]

      [(and (eq? t1 t2)
            (string<? m2 m1))
       (track-warning! s2 (format "require ~.s should come before ~.s" m2 m1))])))

(define-syntax-class root-module-path
  (pattern mod:id
           #:with t 'absolute
           #:with s (symbol->string (syntax-e #'mod))
           #:do [(define mod-sym (syntax->datum #'mod))
                 (case mod-sym
                   [(racket/contract)
                    (track-warning! #'mod "prefer racket/contract/base if possible")]
                   [(syntax/parse)
                    (track-warning! #'mod "prefer syntax/parse/pre if possible")])])
  (pattern mod:string
           #:with t 'relative
           #:with s #'mod))

(define-syntax-class require-spec
  #:datum-literals (combine-in except-in for-syntax only-in prefix-in submod)
  (pattern mod:root-module-path
           #:with t #'mod.t
           #:with s #'mod.s)
  (pattern (for-syntax e:require-spec ...)
           #:with t 'syntax
           #:with s "")
  (pattern (only-in spec:require-spec id ...)
           #:with t #'spec.t
           #:with s #'spec.s)
  (pattern (except-in spec:require-spec id ...)
           #:with t #'spec.t
           #:with s #'spec.s)
  (pattern (prefix-in prefix:id spec:require-spec)
           #:with t #'spec.t
           #:with s #'spec.s)
  (pattern (combine-in)
           #:with t 'absolute
           #:with s ""
           #:do [(track-warning! this-syntax "empty combine-in")])
  (pattern (combine-in spec0:require-spec spec:require-spec ...)
           #:with t #'spec0.t
           #:with s #'spec0.s
           #:do [(check-requires-sorted
                  #'(spec0 spec ...)
                  #'(spec0.s spec.s ...)
                  #'(spec0.t spec.t ...))])
  (pattern (submod spec0:require-spec spec:require-spec ...)
           #:with t #'spec0.t
           #:with s #'spec0.s))

(define-syntax-class require-statement
  #:datum-literals (require)
  (pattern (require e:require-spec ...+)
           #:do [(check-requires-sorted
                  #'(e ...)
                  #'(e.s ...)
                  #'(e.t ...))]))

(define-syntax-class provide-renamed-id
  (pattern id:id)
  (pattern e
           #:do [(track-error! #'e "not an identifier")]))

(define-syntax-class provide-spec
  #:datum-literals (all-defined-out contract-out rename rename-out struct struct-out)
  (pattern id:id
           #:do [(track-provided! #'id)])

  (pattern (all-defined-out)
           #:do [(track-all-provided!)])

  (pattern (contract-out
            ~!
            {~do (push-scope!)} ;; push a scope here so bindings are punted on
            {~or* [{~optional struct} name:id e:expression]
                  [rename name:id provided-name:id e:expression]} ...)
           #:do [(for-each track-provided! (syntax-e #'(name ...)))
                 (for-each track-related-provided! (syntax-e #'(name ...)))
                 (pop-scope!)])

  (pattern (rename-out ~! [to-rename-id:id renamed-id:provide-renamed-id] ...)
           #:do [(for-each track-provided! (syntax-e #'(to-rename-id ...)))])

  (pattern (struct-out ~! struct-id:id)
           #:do [(track-provided! #'struct-id)
                 (track-related-provided! #'struct-id)
                 (cond
                   [(bindings-ref (format-binding "~a" #'struct-id))
                    => (lambda (bi)
                         (for ([stx (in-list (binding-info-related-stxes bi))])
                           (track-provided! stx)))])])

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

(define-syntax-class struct-method-definition
  #:datum-literals (define define/generic)
  (pattern (define/generic local-id:id method-id:id)
           #:do [(track-binding! #'local-id)])
  (pattern (define id:id e:expression)
           #:do [(track-binding! #'id #:check-usages? #f)])
  (pattern (define {~var hdr (function-header #t)} body-e:expression ...+)
           #:do [(define method-name (syntax-e (cadr (syntax-e #'hdr.name))))
                 (track-binding-usage! method-name)
                 ((attribute hdr.pop-scopes!))]))

(define-syntax-class struct-definition
  #:datum-literals (serializable-struct serializable-struct/versions struct struct/contract struct++)
  (pattern ((~or serializable-struct
                 serializable-struct/versions
                 struct
                 struct/contract)
            ~!
            name:id
            (~optional super-id:identifier-expression)
            (~optional version:number)
            (field:struct-field-spec ...)
            (~alt {~optional (~seq #:name name-override:id)}
                  (~and #:mutable struct-mutable)
                  (~seq #:property P:expression PV:expression)
                  (~seq #:methods ~! G:identifier-expression [md ...])
                  e) ...)
           #:do [(define name-stx #'{~? name-override name})
                 (track-binding! name-stx)
                 (track-binding! #'name "~a?" #:check-usages? #f #:related-to name-stx)
                 (define prefix (symbol->string (format-binding "~a" #'name)))
                 (define mutable? (not (null? (syntax->datum #'(struct-mutable ...)))))
                 (for-each (lambda (stx field-mutable?)
                             (track-binding! stx (string-append prefix "-~a") #:related-to name-stx)
                             (when (or mutable? field-mutable?)
                               (track-binding! stx (string-append "set-" prefix "-~a!") #:related-to name-stx)))
                           (syntax-e #'(field.name ...))
                           (map (compose1 not not syntax-e) (syntax-e #'(field.mutable? ...))))
                 ;; Using try-track-struct-usage! here ensures that G
                 ;; and all of its related definitions are tracked as
                 ;; used.
                 (for ([G-stx (in-list (syntax-e #'(G ...)))])
                   (define G-sym (syntax-e G-stx))
                   (try-track-struct-usage! G-sym))
                 (push-scope!)
                 (syntax-parse #'(md ... ...)
                   [(d:struct-method-definition ...) (void)])
                 (pop-scope!)])

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
                             (track-binding! stx (string-append (symbol->string (format-binding "~a" #'name)) "-~a") #:check-usages? #f)
                             (track-binding! stx (string-append (symbol->string (format-binding "set-~a" #'name)) "-~a") #:check-usages? #f)
                             (track-binding! stx (string-append (symbol->string (format-binding "update-~a" #'name)) "-~a") #:check-usages? #f))
                           (syntax-e #'(spec.name ...)))]))

(define extensions
  (for*/list ([path (find-relevant-directories '(review-exts))]
              [bind (in-list ((get-info/full path) 'review-exts))])
    (match-define (list mod predicate-id extension-id) bind)
    (cons
     (dynamic-require mod predicate-id)
     (dynamic-require mod extension-id))))

(define (find-extension stx)
  (findf (λ (ext) ((car ext) stx)) extensions))

(define-syntax-class extension
  (pattern e
           #:when (find-extension this-syntax)
           #:do [(with-handlers ([exn:fail?
                                  (lambda (e)
                                    ((error-display-handler)
                                     (exn-message e)
                                     e))])
                   (parameterize ([current-reviewer
                                   (reviewer
                                    (λ (stx)
                                      (syntax-parse stx
                                        [e:expression #'e]))
                                    track-error!
                                    track-warning!
                                    track-binding!
                                    try-track-struct-usage!
                                    push-scope!
                                    pop-scope!)])
                     ((cdr (find-extension this-syntax)) this-syntax)))]))

(define-syntax-class expression
  #:commit
  (pattern ext:extension)
  (pattern d:definition)
  (pattern s:struct-definition)
  (pattern l:lambda-expression)
  (pattern c:match-expression)
  (pattern c:cond-expression)
  (pattern i:if-expression)
  (pattern l:let-expression)
  (pattern f:for-expression)
  (pattern s:scoping-expression)
  (pattern c:contract-arrow-expression)
  (pattern a:application-expression)
  (pattern I:identifier-expression)
  (pattern S:string)
  (pattern N:number)
  (pattern B:boolean)
  (pattern K:keyword)
  (pattern e))

(define-syntax-class toplevel
  #:commit
  (pattern M:module)
  (pattern m:submodule)
  (pattern r:require-statement)
  (pattern p:provide-statement)
  (pattern e:expression))

(define-syntax-class module
  #:datum-literals (module #%module-begin)
  (pattern (module _name:id _path:id
             (#%module-begin ~! _e:toplevel ...))
           #:do [(check-unused-bindings!)]))

(define-syntax-class submodule
  #:datum-literals (module module+)
  (pattern (module ~! _name _path _e ...)
           #:do [(lint-submodule-syntax!/trampoline this-syntax)])

  (pattern (module+ ~!
             _name:id
             (~do (push-scope!))
             _e:toplevel ...
             (~do (pop-scope!)))))
