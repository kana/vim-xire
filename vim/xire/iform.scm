(define-module vim.xire.iform
  (export
    ; Public API.
    $begin
    $break
    $call
    $const
    $def
    $ex
    $for
    $func
    $gref
    $gset
    $if
    $let
    $lref
    $lset
    $next
    $ret
    $while
    <lvar>
    iform-tag
    iform?
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
    make-func-ctx~
    make-local-ctx~
    make-lvars

    ; Not public, but exported to test.
    ))
(select-module vim.xire.iform)

(use util.list)
(use util.match)
(use vim.xire.util)




;;; The structure of the compiler
;;; =============================
;;;
;;; Xire script is compiled with the following steps:
;;;
;;; Pass 1 (parsing)
;;;     - Xire script is parsed as a list of S expressions.
;;;     - Use of macros are recursively expanded into an intermediate form,
;;;       which is called IForm.
;;;     - This pass returns a list of IForm objects.
;;;       More precisely, a list of <top-stmt> objects.
;;;
;;; Pass Final (code generation)
;;;     - A list of IForm objects is traversed and corresponding Vim script
;;;       is generated.
;;;
;;; More passes will be added between Pass 1 and Pass Final.




;;; Intermediate form of Vim script (IForm)
;;; =======================================
;;;
;;; Vim script is not a good programming language, because it is originated
;;; from Ex commands.  Though it might be a good DSL to edit text data, its
;;; syntax is too arbitrary as a programming language.
;;;
;;; So that Xire script is converted into an IForm object at first.
;;; IForm is designed to:
;;;
;;; - Express a code of Vim script in a unified format.
;;; - Manipulate an intermediate code easily.
;;; - Separate passes of the compilation clearly.


;;; Naming conventions
;;; ------------------
;;;
;;; expr                An iform of an expression.
;;; gvar                A global variable.
;;; iform               An iform.
;;; lvar                A local variable.
;;; name                A symbol.
;;; obj                 A non-iform object.
;;; stmt                An iform of a statement.


;;; Local variables
;;; ---------------
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

(define (lvar-ref++! lvar)
  (inc! (lvar-ref-count lvar)))
(define (lvar-ref--! lvar)
  (dec! (lvar-ref-count lvar)))
(define (lvar-set++! lvar)
  (inc! (lvar-set-count lvar)))
(define (lvar-set--! lvar)
  (dec! (lvar-set-count lvar)))

(define (make-func-ctx~ ctx names)
  ; FIXME: Replace make-func-ctx.
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
  ; FIXME: Replace make-local-ctx with make-lvars and make-local-ctx~.
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
(define (make-local-ctx~ ctx lvars)
  ; FIXME: Replace make-local-ctx with make-lvars and make-local-ctx~.
  (rlet1 new-ctx (copy-ctx ctx)
    (set! (ref new-ctx 'locals)
      (append (map (lambda (v)
                     (cons (lvar-src-name v) v))
                   lvars)
              (ref new-ctx 'locals)))
    ))


;;; Utilities on IForm
;;; ------------------

(define iform?
  ; All iform objects are represented as vectors.
  vector?)

(define (iform-tag iform)
  (vector-ref iform 0))


;;; <top-stmt>
;;; ----------
;;;
;;; <top-stmt> is a <stmt> or one of the following forms:

(define ($def gvar expr)
  ; Define a global variable.
  `#($DEF ,gvar ,expr))


;;; <stmt>
;;; ------
;;;
;;; <stmt> is one of the following forms:

(define ($gset gvar expr)
  ; Modify a global variable.
  `#($GSET ,gvar ,expr))

(define ($let lvars stmt)
  ; Define local variables.
  `#($LET ,lvars ,stmt))

(define ($lset lvar expr)
  ; Modify a local variable.
  `#($LSET ,lvar ,expr))

(define ($begin stmts)
  ; Execute statements sequentially.
  `#($BEGIN ,stmts))

(define ($if expr then-stmt else-stmt)
  ; Branch execution.
  `#($IF ,expr ,then-stmt ,else-stmt))

(define ($while expr stmt)
  ; Loop by a condition.
  `#($WHILE ,expr ,stmt))

(define ($for lvar expr stmt)
  ; Loop by a list.
  `#($FOR ,lvar ,expr ,stmt))

(define ($break)
  ; Exit from the most inner loop.
  `#($BREAK))

(define ($next)
  ; Go to the next step of the most inner loop.
  `#($NEXT))

(define ($ret expr)
  ; Exit from the current function.
  `#($RET ,expr))

; FIXME: Support :try/:catch/:finally in the distant future.

(define ($func func-name args stmt)
  ; Create a function.
  `#($FUNC ,func-name ,args ,stmt))

(define ($ex obj-or-iforms)
  ; Execute an arbitrary Ex command.
  `#($EX ,obj-or-iforms))


;;; <expr>
;;; ------
;;;
;;; <expr> is one of the following forms:

(define ($const obj)
  ; Return a literal.
  `#($CONST ,obj))

(define ($gref gvar)
  ; Reference a global variable.
  `#($GREF ,gvar))

(define ($lref lvar)
  ; Reference a local variable.
  `#($LREF ,lvar))

(define ($call func-expr-or-op-name arg-exprs)
  ; Call a function or a built-in operator.
  `#($CALL ,func-expr-or-op-name ,arg-exprs))




;;; __END__
