(define-module vim.xire
  (export
    ; Public API
    =ex=
    define-xire-expr
    define-xire-stmt
    generate-match-body
    generate-match-pattern
    scheme->ivs
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
    convert-identifier-conventions
    convert-regexp-conventions
    convert-string-conventions
    ctx-type->xire-env-slot-name
    xire-lookup-macro
    xire-register-macro!
    ))
(select-module vim.xire)

(use gauche.parameter)
(use srfi-1)
(use text.tree)
(use util.list)
(use util.match)




;;; Environment
;;; ===========

;; Represent an environment for xire compilation.
(define-class <xire-env> ()
  ([expr-macros  ; contain xire macro bindings for expression context.
     :init-keyword :expr-macros
     :init-form (make-hash-table 'eq?)]
   [stmt-macros  ; contain xire macro bindings for statement context.
     :init-keyword :stmt-macros
     :init-form (make-hash-table 'eq?)]))

;; Represent the current environment for xire compilation.
(define xire-env
  (make-parameter (make <xire-env>)))

;; Copy the current xire environment.
(define (copy-env :optional (env (xire-env)))
  (make <xire-env>
        :expr-macros (hash-table-copy (ref env 'expr-macros))
        :stmt-macros (hash-table-copy (ref env 'stmt-macros))
        ))




;;; Context
;;; =======

;; Represent a context to compile a xire script.
;; The differences between <xire-ctx> and <xire-env> are that:
;;
;; - <xire-env> holds global information such as xire macro bindings.
;; - While <xire-ctx> holds local information, for example, whether a xire
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

;; Look up a xire macro with the NAME from ENV.
;; The macro must be available in a given CTX.
(define (xire-lookup-macro name ctx :optional (env (xire-env)))
  (hash-table-get (ref env (ctx-type->xire-env-slot-name (ref ctx 'type)))
                  name
                  #f))

;; Register a xire macro EXPANDER with the NAME into ENV.
;; The macro will be available in a given context corresponding to CTX-TYPE.
(define (xire-register-macro! name expander ctx-type
                              :optional (env (xire-env)))
  (if (not (or (eq? ctx-type 'expr)
               (eq? ctx-type 'stmt)))
    (errorf "Invalid ctx-type: ~s" ctx-type))
  (hash-table-put! (ref env (ctx-type->xire-env-slot-name ctx-type))
                   name
                   expander))

;; Define new xire macro in the current environment.
;; This is just a syntax sugar for xire-register-macro!.
(define-syntax define-xire-macro
  (syntax-rules ()
    [(_ ctx-type (name form ctx) . body)
     (xire-register-macro! 'name
                           (lambda (form ctx) . body)
                           'ctx-type
                           (xire-env))]))

;; Define new xire expression macro in the current environment.
;; This is a wrapper for define-xire-macro.
(define-syntax define-xire-expr
  (syntax-rules ()
    ; The most low-level form.
    [(_ "internal" name :low ctx [pat . body] ...)
     (define-xire-macro expr (name form ctx)
       (ensure-expr-ctx form ctx)
       (match form
         [pat . body]
         ...))]
    ; Basic form.
    [(_ name :low [pat . body] ...)
     (define-xire-expr name :low ctx [pat . body] ...)]
    ; Basic form with the name of context.
    [(_ name :low ctx [pat . body] ...)
     (define-xire-expr "internal" name :low ctx [pat . body] ...)]))

;; FIXME: define-xire-expr (:high)

;; Define new xire statement macro in the current environment.
;; This is a wrapper for define-xire-macro.
(define-syntax define-xire-stmt
  (syntax-rules ()
    ; The most low-level form.
    [(_ "internal" name :low ctx [pat . body] ...)
     (define-xire-macro stmt (name form ctx)
       (ensure-stmt-ctx form ctx)
       (match form
         [pat . body]
         ...))]
    ; Basic form.
    [(_ name :low [pat1 . body1] [patN . bodyN] ...)
     (define-xire-stmt name :low ctx
       [pat1 . body1]
       [patN . bodyN]
       ...)]
    ; Basic form with the name of context.
    [(_ name :low ctx [pat1 . body1] [patN . bodyN] ...)
     (define-xire-stmt "internal" name :low ctx
       [pat1 . body1]
       [patN . bodyN]
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
        (=ex= 'cmd-name)])]
    ; Shorthand for simple command like :break.
    [(_ name)
     (define-xire-stmt name :low
       [(_)
        (=ex= 'name)])]
    ))

;; FIXME: define-xire-stmt (:high)




;;; Compiler
;;; ========

;; Translate xire script into Vim script.  Xire script is read from INPUT-PORT
;; and resulting Vim script is written into OUTPUT-PORT.
(define (xire-translate input-port output-port
                        :key (env (copy-env))
                             (scheme-env (make-module #f)))
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

;; Compile a xire script FORM then return a resulting Vim script in IVS.
(define (xire-compile form ctx)
  (define (report-syntax-error)
    (errorf "Invalid xire form: ~s" form))
  (match form
    [((? symbol? name) . args)
     (cond
       [(xire-lookup-macro name ctx)
        => (lambda (expander)
             (xire-compile (expander form ctx) ctx))]
       [else
         ; Treat "(foo ...)" form in an expression context as a function call,
         ; where foo is not known as a xire macro.  This convention is to
         ; simplify the compiler implementation.
         (if (expr-ctx? ctx)
           `("("
             ,(convert-identifier-conventions (symbol->string name))
             "("
             ,@(intersperse "," (xire-compile-forms args ctx))
             ")"
             ")")
           (report-syntax-error))])]
    [(_ . _)  ; FORM is already compiled.
     form]
    [_
      (ensure-expr-ctx form ctx)
      (scheme->ivs form)]))

;; Compile a xire script EXPR then return a resulting Vim script in IVS.
;; This is an abbreviated form of xire-compile for typical use.
(define (xire-compile-expr expr ctx)
  (xire-compile expr
                (if (expr-ctx? ctx)
                  ctx
                  (make-expr-ctx ctx))))

;; Compile a list of xire script FORMS then return a resulting Vim script in
;; IVS.  This is an abbreviated form of xire-compile for typical use.
(define (xire-compile-forms forms ctx)
  (map (cut xire-compile <> ctx) forms))




;;; Helper API
;;; ==========

;; Convert identifier conventions between Scheme and Vim script.  For example,
;; Scheme usually uses hyphen (-) to join words in an identifier, while Vim
;; script uses underscore (_) for that purpose.  This procedure converts such
;; conventions.
(define (convert-identifier-conventions scheme-identifier-string)
  ; FIXME: Raise error for invalid characters as Vim script identifier.
  (regexp-replace-all* scheme-identifier-string
                       #/\?$/ "_p"
                       #/!$/ "_x"
                       #/->/ "_to_"
                       #/[-%]/ "_"
                       ))

;; Convert Scheme regexp object into equivalent one in Vim script.
;; Unlike convert-string-conventions, there is no limitation to write regexp.
;; Because external representation of regexp in Scheme is almost same as one
;; in Vim script.
(define (convert-regexp-conventions scheme-regexp)
  (regexp-replace-all* (regexp->string scheme-regexp)
                       #/'/ "''"
                       #/^(.*)$/ "'\\1'"
                       ))

;; Convert Scheme string object into Vim script string notation.
;;
;; In xire script, Vim script strings are written as Scheme strings.  But both
;; notations are not completely equivalent.  So that we put the following
;; limitations on Scheme strings which will be compiled into Vim script:
;;
;; (i) In Scheme strings, only the following backslash-escape notations may be
;;     used:
;;
;;     \\ \" \f \n \r \t \uNNNN \xNN \<whitespace>*<newline><whitespace>*
;;
;;     - All but the last notation are the same as Vim script's ones, so that
;;       external representation of Scheme strings and Vim script strings are
;;       the same if this condition is met.  Therefore we can simply write
;;       Scheme strings as if they are Vim script strings.
;;
;;     - The last notation is processed by Scheme's reader and it is simply
;;       ignored.  So that it may be used too.
;;
;;     - Other notations (\0, \UNNNNNNNN) must not be used.  Because:
;;       - There is no equivalent for \UNNNNNNNN.
;;       - Vim script cannot handle NUL character as is.  Though we can write
;;         "\0" in Vim script, such strings are essentially wrong.  So that it
;;         must not be used.
;;
;; (ii) The following backslash-escape notations in Vim script are not
;;      available in xire script:
;;
;;      (o) \. \.. \... (arbitrary byte, in octal digits)
;;      (x) \x.         (arbitrary byte, in single hex digit)
;;      (X) \X. \X..    (equivalent to \x. and \x..)
;;      (U) \U....      (equivalent to \u....)
;;      (b) \b          (equivalent to \<BS>)
;;      (e) \e          (equivalent to \<Esc>)
;;      (k) \<Key>      (special key sequence)
;;
;;      - (o), (x) and (X): Use "\xNN" instead.
;;      - (b) and (e): Use "\xNN" instead.
;;      - (U): Incompatible with Gauche's "\UNNNNNNNN" notation and it is
;;        rarely used.
;;      - (k): Use (kbd "<Key> ...") form instead.  [KEYSEQ]
;;
;; [KEYSEQ]
;;     In Vim script, it's necessary to write special key sequences in
;;     the following situations:
;;
;;     (A) To denote input:
;;         (a) :execute 'normal!' "\<Key>"
;;         (b) :call feedkeys("\<Key>", 't')
;;         (c) :map <expr> {lhs} "\<Key>"
;;         (d) :map {lhs} <C-r>="\<Key>"<Return>
;;     (B) To compare input:
;;         (a) visualmode() ==# "\<C-v>"
;;         (b) 0 <= index(["\<C-c>", "\<Esc>"], nr2char(getchar()))
;;
;;     But it's possible to write equivalent Vim script if (kbd "<Key> ...")
;;     form is available.  So that it's not a problem even if we cannot write
;;     "\<Key>" directly in xire script.
(define (convert-string-conventions scheme-string)
  ; FIXME: Check the above limitations are met.
  (format "~s" scheme-string))

(define (=ex= . ex-cmd-ivss)
  (define (insert-newline xs)
    (map (lambda (x)
           `(,x "\n"))
         xs))
  (define (insert-space xs)
    (map (lambda (x)
           (if (list? x)
             (intersperse " " x)
             x))
         xs))
  `(()
    ,@(concatenate
        (insert-newline
          (insert-space
            ex-cmd-ivss))))
  )

(define (generate-match-body pat rule)
  (define (shift xs)
    ; (if $cond:expr $then:stmt)
    ; ==> ([$then:stmt . ()] [$cond:expr . $then:stmt] [if . $cond:expr])
    (define (go xs pairs)
      (if (null? (cdr xs))
        (cons (cons (car xs) '()) pairs)
        (go (cdr xs) (cons (cons (car xs) (cadr xs)) pairs))))
    (go xs '()))
  (define (parse-pat pat)
    (define (slot? x)
      (and (symbol? x)
           (#/^\$/ (symbol->string x))))
    (define (convert slot following)
      (let ([slot-name (string->symbol
                         (regexp-replace* (symbol->string slot)
                                          #/^(\$[^:]+):.*$/
                                          "\\1"))]
            [value-name slot]
            [manyp (eq? following '...)]
            [type (string->symbol
                    (regexp-replace* (symbol->string slot)
                                     #/^\$[^:]+:(.*)$/
                                     "\\1"))])
        (list slot-name value-name manyp type)))
    (map (lambda (pair) (convert (car pair) (cdr pair)))
         (filter (compose slot? car) (shift pat))))
  (define (generate-let-binding slot-info)
    (match-let1 (slot-name value-name manyp type) slot-info
      `[,slot-name (transform-value ,value-name ,manyp ',type ctx)]))
  `(let (,@(map generate-let-binding (parse-pat pat)))
     ,@rule))

(define (generate-match-pattern xs)
  (define (escape x)
    (if (and (symbol? x)
             (not (eq? x '...))
             (not (#/^\$/ (symbol->string x))))
      `',x
      x))
  (map escape xs))

(define (scheme->ivs x)
  ; Vim script numbers are 32-bit signed integers.  (cf. :help Number)
  (define MINIMAL-NUMBER (ash -1 31))
  (define MAXIMUM-NUMBER (- (ash 1 31) 1))

  (cond
    [(boolean? x)
     (if x
       1
       0)]
    [(number? x)
     (cond
       [(< x MINIMAL-NUMBER)
        (errorf "Too small number for Vim script: ~s" x)]
       [(< MAXIMUM-NUMBER x)
        (errorf "Too big number for Vim script: ~s" x)]
       [(inexact? x)
        (errorf "Inexact number can not be expressed in Vim script: ~s" x)]
       [(integer? x)
        x]
       ; FIXME: Support floating point numbers in Vim script.
       [else
         (errorf "Invalid number for Vim script: ~s" x)])]
    [(regexp? x)
     (convert-regexp-conventions x)]
    [(string? x)
     (convert-string-conventions x)]
    [(symbol? x)
     (convert-identifier-conventions (symbol->string x))]
    ; List literal is provided by a xire macro.
    ; Dictionary literal is provided by a xire macro.
    ; FIXME: How about Funcref?
    [else
      (errorf "Invalid Scheme object for Vim script: ~s" x)]
    )
  )

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
      [else
        (errorf "Invalid type for transform-value: ~s" type)]))
  (if manyp
    (map transform-form form-or-forms)
    (transform-form form-or-forms)))




(provide "vim/xire")
