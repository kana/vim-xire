(define-module vim.xire.iform
  (export
    ; Public API.
    pass-final

    ; Not public, but exported to test.
    bin-op-table
    iform-tag
    iform?
    make-begin
    make-break
    make-call
    make-const
    make-def
    make-ex
    make-for
    make-func
    make-gref
    make-gset
    make-if
    make-let
    make-lref
    make-lset
    make-next
    make-ret
    make-while
    ))
(select-module vim.xire.iform)

(use util.list)
(use util.match)
(use vim.xire.ivs)  ; FIXME: Migrate necessary procedures into vim.xire.iform.




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


;;; Utilities
;;; ---------

(define iform?
  ; All iform objects are represented as vectors.
  vector?)

(define (iform-tag iform)
  (vector-ref iform 0))


;;; <top-stmt>
;;; ----------
;;;
;;; <top-stmt> is a <stmt> or one of the following forms:

(define (make-def gvar expr)
  ; Define a global variable.
  `#($DEF ,gvar ,expr))


;;; <stmt>
;;; ------
;;;
;;; <stmt> is one of the following forms:

(define (make-gset gvar expr)
  ; Modify a global variable.
  `#($GSET ,gvar ,expr))

(define (make-let lvars exprs stmt)
  ; Define local variables.
  `#($LET ,lvars ,exprs ,stmt))

(define (make-lset lvar expr)
  ; Modify a local variable.
  `#($LSET ,lvar ,expr))

(define (make-begin stmts)
  ; Execute statements sequentially.
  `#($BEGIN ,stmts))

(define (make-if expr then-stmt else-stmt)
  ; Branch execution.
  `#($IF ,expr ,then-stmt ,else-stmt))

(define (make-while expr stmt)
  ; Loop by a condition.
  `#($WHILE ,expr ,stmt))

(define (make-for lvar expr stmt)
  ; Loop by a list.
  `#($FOR ,lvar ,expr ,stmt))

(define (make-break)
  ; Exit from the most inner loop.
  `#($BREAK))

(define (make-next)
  ; Go to the next step of the most inner loop.
  `#($NEXT))

(define (make-ret expr)
  ; Exit from the current function.
  `#($RET ,expr))

; FIXME: Support :try/:catch/:finally in the distant future.

(define (make-func func-name arg-names stmt)
  ; Create a function.
  `#($FUNC ,func-name ,arg-names ,stmt))

(define (make-ex obj-or-iforms)
  ; Execute an arbitrary Ex command.
  `#($EX ,obj-or-iforms))


;;; <expr>
;;; ------
;;;
;;; <expr> is one of the following forms:

(define (make-const obj)
  ; Return a literal.
  `#($CONST ,obj))

(define (make-gref gvar)
  ; Reference a global variable.
  `#($GREF ,gvar))

(define (make-lref lvar)
  ; Reference a local variable.
  `#($LREF ,lvar))

(define (make-call func-expr-or-op-name arg-exprs)
  ; Call a function or a built-in operator.
  `#($CALL ,func-expr-or-op-name ,arg-exprs))




;;; Pass Final (code generation)
;;; ============================

;;; Misc. utilities
;;; ---------------

(define-constant bin-op-table
  '((or . "||")
    (and . "&&")
    (!= . "!=")
    (!=# . "!=#")
    (!=? . "!=?")
    (!~ . "!~")
    (!~# . "!~#")
    (!~? . "!~?")
    (< . "<")
    (<# . "<#")
    (<= . "<=")
    (<=# . "<=#")
    (<=? . "<=?")
    (<? . "<?")
    (== . "==")
    (==# . "==#")
    (==? . "==?")
    (=~ . "=~")
    (=~# . "=~#")
    (=~? . "=~?")
    (> . ">")
    (># . ">#")
    (>= . ">=")
    (>=# . ">=#")
    (>=? . ">=?")
    (>? . ">?")
    (is . "is")
    (is# . "is#")
    (is? . "is?")
    (isnot . "isnot")
    (isnot# . "isnot#")
    (isnot? . "isnot?")
    (+ . "+")
    (- . "-")
    (.. . ".")
    (* . "*")
    (/ . "/")
    (% . "%")))

(define (bin-op-info op-name)
  (assq op-name bin-op-table))


;;; State for code generation
;;; -------------------------

(define-class <pass-final/state> ()
  ([in-scriptp
     :init-keyword :in-scriptp
     :init-value #t]
   [in-funcp
     :init-keyword :in-funcp
     :init-value #f]
   [func-args
     :init-keyword :func-args
     :init-value '()]  ; Alist of (original-name . new-name).
   [lvars
     :init-keyword :lvars
     :init-value '()]))  ; Alist of (original-name . new-name).


;;; Entry point
;;; -----------

(define (pass-final iforms)
  (map (cut pass-final/rec <> (make <pass-final/state>))
       iforms))

(define (pass-final/rec iform state)
  (let gen ([iform iform]
            [state state])
    (match iform
      [#('$CONST obj)
        (if (or (boolean? obj) (number? obj) (regexp? obj) (string? obj))
          (scheme-object->vim-script-notation obj)
          (errorf "$CONST contains an invalid object: ~s" obj))]
      [#('$GREF gvar)
        (convert-identifier-conventions
          (symbol->string gvar))]
      [#('$LREF lvar)
        (scheme-object->vim-script-notation lvar)]
      [#('$CALL (? iform? func-expr) arg-exprs)
        (list (gen func-expr state)
              "("
              (intersperse "," (map (cut gen <> state) arg-exprs))
              ")")]
      [#('$CALL 'if (cond-expr then-expr else-expr))
        (list "("
              (gen cond-expr state)
              " ? "
              (gen then-expr state)
              " : "  ; To parse r?s:t as (r)?(s):(t) not (r)?(s:t).
              (gen else-expr state)
              ")")]
      [#('$CALL (= bin-op-info op) (left-expr right-expr))
        (=> next)
        (unless op
          (next))
        (list "("
              (gen left-expr state)
              " "
              (cdr op)
              " "
              (gen right-expr state)
              ")")]
      [else
        (errorf "This iform is not valid: ~s" iform)])))




;;; __END__
