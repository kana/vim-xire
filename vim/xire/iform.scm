(define-module vim.xire.iform
  (export
    ; Public API.
    $begin
    $break
    $call
    $const
    $def
    $ex
    $for~
    $func
    $gref
    $gset
    $if
    $let
    $let~
    $lref
    $lref~
    $lset
    $lset~
    $next
    $ret
    $while
    <lvar>
    iform-tag
    iform?
    lvar-init-expr
    lvar-new-name
    lvar-ref++!
    lvar-ref--!
    lvar-ref-count
    lvar-set++!
    lvar-set--!
    lvar-set-count
    lvar-src-name

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

(define-class <lvar> ()
  ((src-name  ; The original name of this variable in source code.
     :init-keyword :src-name
     :getter lvar-src-name)
   (new-name  ; A new name of this variable for resulting Vim script.
     :init-keyword :new-name
     :getter lvar-new-name)
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

(define ($let lvars exprs stmt)
  ; Define local variables.
  `#($LET ,lvars ,exprs ,stmt))

(define ($let~ lvars stmt)
  ; FIXME: Replace $LET.
  ; Define local variables.
  `#($LET~ ,lvars ,stmt))

(define ($lset lvar expr)
  ; Modify a local variable.
  `#($LSET ,lvar ,expr))

(define ($lset~ lvar expr)
  ; FIXME: Replace $LSET.
  ; Modify a local variable.
  `#($LSET~ ,lvar ,expr))

(define ($begin stmts)
  ; Execute statements sequentially.
  `#($BEGIN ,stmts))

(define ($if expr then-stmt else-stmt)
  ; Branch execution.
  `#($IF ,expr ,then-stmt ,else-stmt))

(define ($while expr stmt)
  ; Loop by a condition.
  `#($WHILE ,expr ,stmt))

(define ($for~ lvar expr stmt)
  ; FIXME: Replace $FOR.
  ; Loop by a list.
  `#($FOR~ ,lvar ,expr ,stmt))

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

(define ($func func-name arg-names stmt)
  ; Create a function.
  `#($FUNC ,func-name ,arg-names ,stmt))

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

(define ($lref~ lvar)
  ; FIXME: Replace $LREF.
  ; Reference a local variable.
  `#($LREF~ ,lvar))

(define ($call func-expr-or-op-name arg-exprs)
  ; Call a function or a built-in operator.
  `#($CALL ,func-expr-or-op-name ,arg-exprs))




;;; __END__
