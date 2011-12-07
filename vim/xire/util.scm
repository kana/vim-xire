(define-module vim.xire.util
  (export
    ; Public API.
    convert-identifier-conventions
    convert-key-sequence-conventions
    convert-regexp-conventions
    convert-string-conventions
    defexpr
    defstmt
    scheme-object->vim-script-notation

    ; Semi-public API for advanced usage.
    <lvar>
    <xire-ctx>
    <xire-env>
    copy-ctx
    copy-env
    defmacro
    ensure-expr-ctx
    ensure-stmt-ctx
    expr-ctx?
    func-ctx?
    lvar-arg-name
    lvar-init-expr
    lvar-new-name
    lvar-ref++!
    lvar-ref--!
    lvar-ref-count
    lvar-set++!
    lvar-set--!
    lvar-set-count
    lvar-src-name
    make-expr-ctx
    make-func-ctx
    make-local-ctx
    make-lvars
    make-root-ctx
    make-stmt-ctx
    script-ctx?
    stmt-ctx?
    xire-env

    ; Not public, but exported to test.
    ctx-type->xire-env-slot-name
    generate-expanded-form-of-high-level-macro
    generate-match-body
    generate-match-pat
    xire-lookup-macro
    xire-register-macro!
    ))
(select-module vim.xire.util)

(use gauche.parameter)
(use util.match)




;;; Object conversion from Scheme to Vim script
;;; ===========================================

(define (scheme-object->vim-script-notation x)
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
    ; List literal is provided by a Xire macro.
    ; Dictionary literal is provided by a Xire macro.
    ; FIXME: How about Funcref?
    [else
      (errorf "Invalid Scheme object for Vim script: ~s" x)]
    )
  )

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
;; In Xire script, Vim script strings are written as Scheme strings.  But both
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
;;      available in Xire script:
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
;;     "\<Key>" directly in Xire script.
(define (convert-string-conventions scheme-string)
  ; FIXME: Check the above limitations are met.
  (format "~s" scheme-string))

;; Translate a key sequence string S into equivalent Vim script string.
;; For example, "ixyx<BS>z" in Scheme string will be translated into
;; "ixyx\<BS>z" in Vim script string.
;;
;; The format of a key sequence string S is almost same as {rhs} for
;; :map-family in Vim script.  Note that we define the following conventions
;; to simplify writing Xire script and the compiler implementation:
;;
;; (a) Don't use ``\'' directly.  Use ``<Bslash>'' instead.  Because ``\'' is
;;     processed by Scheme reader, so that we have to escape ``\'' and such
;;     escaping makes script unreadable.
(define (convert-key-sequence-conventions s)
  (string-append
    "\""
    (regexp-replace-all*
      s
      #/<[^<>]+>/ "\\\\\\0"
      #/"/ "\\\\\""
      )
    "\""))




;;; Local variables
;;; ===============
;;;
;;; NB: <lvar> mostly represents a local variable, but it also represents an
;;; argument to a function.

(define-class <lvar> ()
  ((src-name  ; The original name of this variable in source code.
     :init-keyword :src-name
     :getter lvar-src-name)
   (new-name  ; A new name of this variable for resulting Vim script.
     :init-keyword :new-name
     :getter lvar-new-name)
   (arg-name  ; A name to declare this variable as an argument to a function.
     :init-keyword :arg-name
     :getter lvar-arg-name
     :init-value #f)
   (init-expr  ; An expression for the initial value of this variable.
     :init-keyword :init-expr
     :getter lvar-init-expr)
   (ref-count  ; The total number of places which refer this variable.
     :init-keyword :ref-count
     :accessor lvar-ref-count
     :init-value 0)
   (set-count  ; The total number of places which modify this variable.
     :init-keyword :set-count
     :accessor lvar-set-count
     :init-value 0)))

(define-method object-equal? ((v1 <lvar>) (v2 <lvar>))
  (every (lambda (slot)
           (let1 name (slot-definition-name slot)
             (cond
               [(and (slot-bound? v1 name)
                     (slot-bound? v2 name))
                (equal?
                  (ref v1 name)
                  (ref v2 name))]
               [else
                 (and (not (slot-bound? v1 name))
                      (not (slot-bound? v2 name)))])))
         (class-slots <lvar>)))

(define (lvar-ref++! lvar)
  (inc! (lvar-ref-count lvar)))
(define (lvar-ref--! lvar)
  (dec! (lvar-ref-count lvar)))
(define (lvar-set++! lvar)
  (inc! (lvar-set-count lvar)))
(define (lvar-set--! lvar)
  (dec! (lvar-set-count lvar)))




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

(define (ctx-type->xire-env-slot-name ctx-type)
  (cond
    [(eq? ctx-type 'expr)
     'expr-macros]
    [(eq? ctx-type 'stmt)
     'stmt-macros]
    [else
      (errorf "Invalid context type: ~s" ctx-type)]))




;;; Context
;;; =======

;; Represent a context to compile a Xire script.
;; The differences between <xire-ctx> and <xire-env> are that:
;;
;; - <xire-env> holds global information such as Xire macro bindings.
;; - While <xire-ctx> holds local information, for example, whether a Xire
;;   script being compiled is a statement or not.
(define-class <xire-ctx> ()
  ([type  ; The type of a form being compiled -- stmt or expr.
     :init-keyword :type
     :init-value 'stmt]
   [in-scriptp  ; #t if a form in a scirpt file being compiled; otherwise, #f.
     :init-keyword :in-scriptp
     :init-value #t]
   [in-funcp  ; #t to compile forms in a function; otherwise, #f.
     :init-keyword :in-funcp
     :init-value #f]
   [func-args  ; A list of (src-name . lvar) for arguments to a function.
     :init-keyword :func-args
     :init-value '()]
   [locals  ; A list of (src-name . lvar) for local variables.
     :init-keyword :locals
     :init-value '()]
   ))

(define (copy-ctx ctx)
  (let1 new-ctx (make <xire-ctx>)
    (for-each
      (lambda (slot-name)
        (set! (ref new-ctx slot-name) (ref ctx slot-name)))
      (map slot-definition-name (class-direct-slots <xire-ctx>)))
    new-ctx))

(define (make-root-ctx :key (in-scriptp #t))
  (make <xire-ctx> :in-scriptp in-scriptp))
(define (make-stmt-ctx ctx)
  (define new-ctx (copy-ctx ctx))
  (set! (ref new-ctx 'type) 'stmt)
  new-ctx)
(define (make-expr-ctx ctx)
  (define new-ctx (copy-ctx ctx))
  (set! (ref new-ctx 'type) 'expr)
  new-ctx)
(define (make-func-ctx ctx names)
  ; NB: Though :function can be written in the body of a :function,
  ;     Vim script does not have lexical scope.  So that nested function
  ;     definition is equivalent to independent function definitions.
  ;     Therefore the compiler does not care about nested functions.
  (define new-ctx (copy-ctx ctx))
  (set! (ref new-ctx 'in-funcp) #t)
  (set! (ref new-ctx 'func-args)
        (map (lambda (n)
               (define n% (string->symbol
                            (convert-identifier-conventions
                              (symbol->string n))))
               (cons n
                     (make <lvar>
                           :src-name n
                           :new-name (if (eq? n '...)
                                       'a:000
                                       (string->symbol #`"a:,n%"))
                           :arg-name n%)))
             names))
  new-ctx)
(define (make-lvars names vals ctx)
  (define (generate-new-name ctx)
    (cond  ; The order of clauses is important.
      [(func-ctx? ctx)
       ; Xire script doesn't provide any way to define function-local
       ; variables except "let" family.  And it's not usual to access
       ; function-local variables from other context.  So that it's not
       ; necessary to take care on name collision.
       (gensym "L")]
      [(script-ctx? ctx)
       ; There is a chance of name collision between variables explicitly
       ; defined with "define" and variables implicitly defined with "let"
       ; family.  To avoid unexpected name collision, generate variable name
       ; with a prefix which, probably, users will not use.
       (gensym "s:__L")]
      [else
        (error "Lexical variables are not available in this context.")]))
  (map (lambda (n v)
         (make <lvar>
               :src-name n
               :new-name (generate-new-name ctx)
               :init-expr v))
       names
       vals))
(define (make-local-ctx ctx lvars)
  (rlet1 new-ctx (copy-ctx ctx)
    (set! (ref new-ctx 'locals)
      (append (map (lambda (v)
                     (cons (lvar-src-name v) v))
                   lvars)
              (ref new-ctx 'locals)))
    ))

(define (stmt-ctx? ctx)
  (eq? (ref ctx 'type) 'stmt))
(define (expr-ctx? ctx)
  (eq? (ref ctx 'type) 'expr))
(define (func-ctx? ctx)
  (ref ctx 'in-funcp))
(define (script-ctx? ctx)
  (ref ctx 'in-scriptp))

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
(define-syntax defmacro
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
  `(defmacro ,ctx-type (,name form ctx)
     ,(cond
        [(eq? ctx-type 'stmt)
         '(ensure-stmt-ctx form ctx)]
        [(eq? ctx-type 'expr)
         '(ensure-expr-ctx form ctx)]
        [else
         (errorf "Invalid context type: ~s" ctx-type)])
     (match form
       ,@(map generate-match-clause clauses))))

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

;; Define new Xire expression macro in the current environment.
;; This is a wrapper for defmacro.
(define-syntax defexpr
  (syntax-rules ()
    [(_ name . args)
     (define-high-level-xire-macro name expr . args)]
    ))

;; Define new Xire statement macro in the current environment.
;; This is a wrapper for defmacro.
(define-syntax defstmt
  (syntax-rules ()
    ; Normal form.
    [(_ name [pat1 . rule1] [patN . ruleN] ...)
     (define-high-level-xire-macro name stmt
       [pat1 . rule1]
       [patN . ruleN]
       ...)]
    ; Shorthand for simple command like :quit and :quit!.
    [(_ name :!)
     (let1 name! (string->symbol #`",'|name|!")
       (defstmt name)
       (eval `(defstmt ,name!)  ; FIXME: Refine.
             (current-module)))]
    ; Shorthand for simple command like :break with different macro name.
    [(_ name cmd-name)
     (defstmt name
       [(_)
        ($ex '(cmd-name))])]
    ; Shorthand for simple command like :break.
    [(_ name)
     (defstmt name
       [(_)
        ($ex '(name))])]
    ))




;;; __END__
