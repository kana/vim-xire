(define-module vim.xire.compiler
  (export
    ; Public API
    define-xire-expr
    define-xire-stmt
    generate-match-body
    generate-match-pat
    transform-value
    xire-translate

    ; Semi-public API for advanced usage.
    <xire-ctx>
    <xire-env>
    copy-ctx
    copy-env
    define-xire-macro
    ensure-expr-ctx
    ensure-stmt-ctx
    expr-ctx?
    make-expr-ctx
    make-stmt-ctx
    make-toplevel-ctx
    stmt-ctx?
    toplevel-ctx?
    xire-compile
    xire-compile-expr
    xire-compile-forms
    xire-env

    ; Not public, but exported to test.
    ctx-type->xire-env-slot-name
    generate-expanded-form-of-high-level-macro
    xire-lookup-macro
    xire-register-macro!

    ; Not public, but exported to avoid some problems.
    match  ; See [MATCHQ].
    ))
(select-module vim.xire.compiler)

(use gauche.parameter)
(use srfi-1)
(use text.tree)
(use util.list)
(use util.match)
(use vim.xire.ivs)




;;; Environment
;;; ===========

;; Represent an environment for Xire compilation.
(define-class <xire-env> ()
  ([expr-macros  ; contain Xire macro bindings for expression context.
     :init-keyword :expr-macros
     :init-form (make-hash-table 'eq?)]
   [stmt-macros  ; contain Xire macro bindings for statement context.
     :init-keyword :stmt-macros
     :init-form (make-hash-table 'eq?)]))

;; Represent the current environment for Xire compilation.
(define xire-env
  (make-parameter (make <xire-env>)))

;; Copy the current Xire environment.
(define (copy-env :optional (env (xire-env)))
  (make <xire-env>
        :expr-macros (hash-table-copy (ref env 'expr-macros))
        :stmt-macros (hash-table-copy (ref env 'stmt-macros))
        ))




;;; Context
;;; =======

;; Represent a context to compile a Xire script.
;; The differences between <xire-ctx> and <xire-env> are that:
;;
;; - <xire-env> holds global information such as Xire macro bindings.
;; - While <xire-ctx> holds local information, for example, whether a Xire
;;   script being compiled is a top-level statement or not.
(define-class <xire-ctx> ()
  ([type  ; The type of a form being compiled -- statement, expression, etc.
     :init-keyword :type
     :init-value 'stmt]
   [toplevelp
     :init-keyword :toplevelp
     :init-value #t]
   ))

(define (copy-ctx ctx)
  (apply make
         <xire-ctx>
         (concatenate
           (map
             (lambda (slot-name)
               `(:type ,(ref ctx slot-name)))
             (map slot-definition-name (class-direct-slots <xire-ctx>))
             ))
         ))

(define (make-toplevel-ctx)
  (make <xire-ctx>))
(define (make-stmt-ctx ctx)
  (define new-ctx (copy-ctx ctx))
  (set! (ref new-ctx 'type) 'stmt)
  (set! (ref new-ctx 'toplevelp) #f)
  new-ctx)
(define (make-expr-ctx ctx)
  (define new-ctx (copy-ctx ctx))
  (set! (ref new-ctx 'type) 'expr)
  (set! (ref new-ctx 'toplevelp) #f)
  new-ctx)

(define (toplevel-ctx? ctx)
  (ref ctx 'toplevelp))
(define (stmt-ctx? ctx)
  (eq? (ref ctx 'type) 'stmt))
(define (expr-ctx? ctx)
  (eq? (ref ctx 'type) 'expr))

(define (ensure-stmt-ctx form ctx)
  (unless (stmt-ctx? ctx)
    (errorf "Invalid form in a statement context: ~s" form)))
(define (ensure-expr-ctx form ctx)
  (unless (expr-ctx? ctx)
    (errorf "Invalid form in an expression context: ~s" form)))




;;; Xire macros
;;; ===========
;;;
;;; Conventions
;;; -----------
;;;
;;; For procedures in this section,
;;;
;;; - Argument called NAME is a symbol.
;;; - Argument called EXPANDER is a procedure which takes two arguments,
;;;   the form to expand and the context to compile the form.

(define (ctx-type->xire-env-slot-name ctx-type)
  (cond
    [(eq? ctx-type 'expr)
     'expr-macros]
    [(eq? ctx-type 'stmt)
     'stmt-macros]
    [else
      (errorf "Invalid context type: ~s" ctx-type)]))

;; Look up a Xire macro with the NAME from ENV.
;; The macro must be available in a given CTX.
(define (xire-lookup-macro name ctx :optional (env (xire-env)))
  (hash-table-get (ref env (ctx-type->xire-env-slot-name (ref ctx 'type)))
                  name
                  #f))

;; Register a Xire macro EXPANDER with the NAME into ENV.
;; The macro will be available in a given context corresponding to CTX-TYPE.
(define (xire-register-macro! name expander ctx-type
                              :optional (env (xire-env)))
  (if (not (or (eq? ctx-type 'expr)
               (eq? ctx-type 'stmt)))
    (errorf "Invalid ctx-type: ~s" ctx-type))
  (hash-table-put! (ref env (ctx-type->xire-env-slot-name ctx-type))
                   name
                   expander))

;; Define new Xire macro in the current environment.
;; This is just a syntax sugar for xire-register-macro!.
(define-syntax define-xire-macro
  (syntax-rules ()
    [(_ ctx-type (name form ctx) . body)
     (xire-register-macro! 'name
                           (lambda (form ctx) . body)
                           'ctx-type
                           (xire-env))]))

(define-macro (define-high-level-xire-macro . args)
  (apply generate-expanded-form-of-high-level-macro (unwrap-syntax args)))

(define (generate-expanded-form-of-high-level-macro name ctx-type . clauses)
  (define (generate-match-clause clause)
    (match-let1 (pat . body) clause
      `[,(generate-match-pat pat)
        ,(generate-match-body pat body)]))
  `(define-xire-macro ,ctx-type (,name form ctx)
     ,(cond
        [(eq? ctx-type 'stmt)
         '(ensure-stmt-ctx form ctx)]
        [(eq? ctx-type 'expr)
         '(ensure-expr-ctx form ctx)]
        [else
         (errorf "Invalid context type: ~s" ctx-type)])
     (match form
       ,@(map generate-match-clause clauses))))

;; Define new Xire expression macro in the current environment.
;; This is a wrapper for define-xire-macro.
(define-syntax define-xire-expr
  (syntax-rules ()
    [(_ name :low . args)
     (%define-xire-expr-low name . args)]
    [(_ name . args)
     (define-high-level-xire-macro name expr . args)]
    ))

(define-syntax %define-xire-expr-low
  (syntax-rules ()
    ; The most low-level form.
    [(_ "internal" name ctx [pat . body] ...)
     (define-xire-macro expr (name form ctx)
       (ensure-expr-ctx form ctx)
       (match form
         [pat . body]
         ...))]
    ; Basic form.
    [(_ name [pat . body] ...)
     (%define-xire-expr-low name ctx [pat . body] ...)]
    ; Basic form with the name of context.
    [(_ name ctx [pat . body] ...)
     (%define-xire-expr-low "internal" name ctx [pat . body] ...)]
    ))

;; Define new Xire statement macro in the current environment.
;; This is a wrapper for define-xire-macro.
(define-syntax define-xire-stmt
  (syntax-rules ()
    ; Normal form of low-level macro.
    [(_ name :low . args)
     (%define-xire-stmt-low name . args)]
    ; High-level macro.
    [(_ name [pat1 . rule1] [patN . ruleN] ...)
     (define-high-level-xire-macro name stmt
       [pat1 . rule1]
       [patN . ruleN]
       ...)]
    ; Shorthand for simple command like :quit and :quit!.
    [(_ name :!)
     (let1 name! (string->symbol #`",'|name|!")
       (define-xire-stmt name)
       (eval `(define-xire-stmt ,name!)  ; FIXME: Refine.
             (current-module)))]
    ; Shorthand for simple command like :break with different macro name.
    [(_ name cmd-name)
     (define-xire-stmt name :low
       [(_)
        (IVS (S (Q 'cmd-name)))])]
    ; Shorthand for simple command like :break.
    [(_ name)
     (define-xire-stmt name :low
       [(_)
        (IVS (S (Q 'name)))])]
    ))

(define-syntax %define-xire-stmt-low
  (syntax-rules ()
    ; The most low-level form.
    [(_ "internal" name ctx [pat . body] ...)
     (define-xire-macro stmt (name form ctx)
       (ensure-stmt-ctx form ctx)
       (match form
         [pat . body]
         ...))]
    ; Basic form.
    [(_ name [pat1 . body1] [patN . bodyN] ...)
     (%define-xire-stmt-low name ctx
       [pat1 . body1]
       [patN . bodyN]
       ...)]
    ; Basic form with the name of context.
    [(_ name ctx [pat1 . body1] [patN . bodyN] ...)
     (%define-xire-stmt-low "internal" name ctx
       [pat1 . body1]
       [patN . bodyN]
       ...)]
    ))




;;; Compiler
;;; ========

;; Translate Xire script into Vim script.  Xire script is read from INPUT-PORT
;; and resulting Vim script is written into OUTPUT-PORT.
(define (xire-translate input-port output-port
                        :key (env (copy-env))
                             (scheme-env (let1 m (make-module #f)
                                           (eval '(use vim.xire) m)
                                           m)))
  (define compiled-vim-script-tree (list))
  (define (finish)
    (write-tree (reverse compiled-vim-script-tree) output-port))
  (define ctx (make-toplevel-ctx))

  (eval '(extend user) scheme-env)
  (parameterize ([xire-env env])
    (let loop ()
      (match (read input-port)
        [(? eof-object?)
         (finish)]
        [(and ((or 'define-xire-expr
                   'define-xire-macro
                   'define-xire-stmt)
               . _)
              form)
         (eval form scheme-env)
         (loop)]
        [('scheme . scheme-exprs)
         (eval `(begin ,@scheme-exprs) scheme-env)
         (loop)]
        [(and (name . _) form)
         (push! compiled-vim-script-tree (xire-compile form ctx))
         (loop)]))))

;; Compile a Xire script FORM then return a resulting Vim script in IVS.
(define (xire-compile form ctx)
  (define (report-syntax-error)
    (errorf "Invalid Xire form: ~s" form))
  (match form
    [((? symbol? name) . args)
     (cond
       [(xire-lookup-macro name ctx)
        => (lambda (expander)
             (xire-compile (expander form ctx) ctx))]
       [else
         ; Treat "(foo ...)" form in an expression context as a function call,
         ; where foo is not known as a Xire macro.  This convention is to
         ; simplify the compiler implementation.
         (if (expr-ctx? ctx)
           (IVS (E (Q "(")
                   name
                   (Q "(")
                   (apply E (intersperse (Q ",")
                                         (xire-compile-forms args ctx)))
                   (Q ")")
                   (Q ")")))
           (report-syntax-error))])]
    [(_ . _)  ; FORM is already compiled.
     form]
    [(? (cut is-a? <> <ivs>) form)  ; FORM is already compiled.
     form]
    [_
      (ensure-expr-ctx form ctx)
      (IVS (E form))]))

;; Compile a Xire script EXPR then return a resulting Vim script in IVS.
;; This is an abbreviated form of xire-compile for typical use.
(define (xire-compile-expr expr ctx)
  (xire-compile expr
                (if (expr-ctx? ctx)
                  ctx
                  (make-expr-ctx ctx))))

;; Compile a list of Xire script FORMS then return a resulting Vim script in
;; IVS.  This is an abbreviated form of xire-compile for typical use.
(define (xire-compile-forms forms ctx)
  (map (cut xire-compile <> ctx) forms))




;;; Helper API
;;; ==========

(define (generate-match-body pat rule)
  (define (parse-pat pat)
    (define (ellipsis? symbol)
      (eq? symbol '...))
    (define (car-followed-by-ellipsis? pat)
      (and (pair? (cdr pat))
           (ellipsis? (cadr pat))))
    (define (try-pat->slot pat)
      (and (symbol? pat)
           (#/^\$([^:]+):([^:]+)/ (symbol->string pat))))
    (define (go pat manyp slot-data)
      (cond
        [(null? pat)
         slot-data]
        [(pair? pat)
         (go (car pat)
             (or manyp (car-followed-by-ellipsis? pat))
             (go (cdr pat) manyp slot-data))]
        [(try-pat->slot pat) =>
         (lambda (m)
           (cons (list (string->symbol #`"$,(m 1)")
                       pat
                       manyp
                       (string->symbol (m 2)))
                 slot-data))]
        [else
         slot-data]))
    (go pat #f '()))
  (define (generate-let-binding slot-info)
    (match-let1 (slot-name value-name manyp type) slot-info
      `[,slot-name (transform-value ,value-name ,manyp ',type ctx)]))
  `(let (,@(map generate-let-binding (parse-pat pat)))
     ,@rule))

(define (generate-match-pat pat)
  (define (escape x)
    (if (and (symbol? x)
             (not (eq? x '_))
             (not (eq? x '...))
             (not (#/^\$/ (symbol->string x))))
      `',x
      x))
  (map escape pat))

;; Code which defines high-level Xire macro has to bind MATCH to the macro
;; defined in util.match, because the syntax to define high-level Xire macro
;; is implemented with DEFINE-MACRO (built-in) and MATCH (util.match).  This
;; restriction is a bit annoying for development.
;;
;; [MATCHQ] NB: To avoid writing extra (use util.match) in user code,
;; MATCH (vim.xire.compiler) is bound to the same macro as MATCH (util.match),
;; and MATCH (vim.xire.compiler) is exported.
(define match match)

(define (transform-value form-or-forms manyp type upper-ctx)
  (define (transform-form form)
    (cond
      [(eq? type 'stmt)
       (xire-compile form
                     (if (stmt-ctx? upper-ctx)
                       upper-ctx
                       (make-stmt-ctx upper-ctx)))]
      [(eq? type 'expr)
       (xire-compile-expr form upper-ctx)]
      [(eq? type 'form)
       form]
      [else
        (errorf "Invalid type for transform-value: ~s" type)]))
  (if manyp
    (map transform-form form-or-forms)
    (transform-form form-or-forms)))




;;; __END__
