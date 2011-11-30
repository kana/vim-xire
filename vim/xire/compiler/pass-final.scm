(define-module vim.xire.compiler.pass-final
  (export
    ; Public API.
    pass-final

    ; Not public, but exported to test.
    <pass-final/state>
    bin-op-table
    derive-state
    un-op-table
    ))
(select-module vim.xire.compiler.pass-final)

(use util.list)
(use util.match)
(use vim.xire.iform)
(use vim.xire.util)




;;; Misc. utilities
;;; ===============

(define-constant un-op-table
  '((not . "!")
    (1- . "-")
    (1+ . "+")))

(define (un-op-info op-name)
  (assq op-name un-op-table))

(define (bin-op-info op-name)
  (assq op-name bin-op-table))

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

(define (make-tmp-name state)
  (cond
    [(ref state 'in-funcp)
      ; Xire script doesn't provide any way to define function-local
      ; variables except "let" family.  And it's not usual to access
      ; function-local variables from other context.  So that it's not
      ; necessary to take care on name collision.
      (gensym "L")]
    [(ref state 'in-scriptp)
     ; There is a chance of name collision between variables explicitly
     ; defined with "define" and variables implicitly defined with "let"
     ; family.  To avoid unexpected name collision, generate variable name
     ; with a prefix which, probably, users will not use.
     (gensym "s:__L")]
    [else
      (error "Lexical variables are not available in this context.")]))

(define (rename-var name state)
  (let1 orig-name&new-name (or (assq name (ref state 'lvars))
                               (assq name (ref state 'func-args)))
    (if orig-name&new-name
      (cdr orig-name&new-name)
      (errorf "Variable is not defined: ~s" name))))




;;; State for code generation
;;; =========================

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

(define (derive-state base-state . args)
  (let1 new-state (make <pass-final/state>)
    (for-each
      (lambda (slot-name)
        (set! (ref new-state slot-name) (ref base-state slot-name)))
      (map slot-definition-name (class-direct-slots <pass-final/state>)))
    (let go ([slot-name (car args)]
             [slot-value (cadr args)]
             [args (cddr args)])
      (set! (ref new-state slot-name) slot-value)
      (unless (null? args)
        (go (car args)
            (cadr args)
            (cddr args))))
    new-state))




;;; Entry point
;;; ===========

(define (pass-final iforms :optional (state (make <pass-final/state>)))
  (map (cut pass-final/rec <> state)
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
        (lvar-new-name lvar)]
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
      [#('$CALL (= un-op-info op) (expr))
        (=> next)
        (unless op
          (next))
        (list "("
              (cdr op)
              (gen expr state)
              ")")]
      [#('$CALL 'ref (collection-expr index-expr))
        (list "("
              (gen collection-expr state)
              "["
              (gen index-expr state)
              "]"
              ")")]
      [#('$CALL 'slice (collection-expr from-expr until-expr))
        (list "("
              (gen collection-expr state)
              "["
              (if from-expr
                (gen from-expr state)
                '())
              " : "  ; To parse l[s:x] as l[(s):x] not l[(s:x)].
              (if until-expr
                (gen until-expr state)
                '())
              "]"
              ")")]
      [#('$CALL '-> (dict-expr name))
        (list "("
              (gen dict-expr state)
              "."
              name
              ")")]
      [#('$CALL 'list (exprs ...))
        (list "["
              (intersperse "," (map (cut gen <> state) exprs))
              "]")]
      [#('$CALL 'dict ((key-exprs ...) (val-exprs ...)))
        (list "{"
              (intersperse
                ","
                (map (lambda (key-expr val-expr)
                       (list (gen key-expr state)
                             " : "  ; To parse {s:x} as {(s):x} not {(s:x)}.
                             (gen val-expr state)))
                     key-exprs
                     val-exprs))
              "}")]
      [#((or '$DEF '$GSET) gvar expr)
        (list "let"
              " "
              (convert-identifier-conventions (symbol->string gvar))
              "="
              (gen expr state)
              "\n")]
      [#('$LSET lvar expr)
        (list "let"
              " "
              (lvar-new-name lvar)
              "="
              (gen expr state)
              "\n")]
      [#('$LET (lvars ...) stmt)
        (list (map (lambda (lvar)
                     (list "let"
                           " "
                           (lvar-new-name lvar)
                           "="
                           (gen (lvar-init-expr lvar) state)
                           "\n"))
                   lvars)
              (gen stmt state))]
      [#('$BEGIN (stmts ...))
        (map (cut gen <> state) stmts)]
      [#('$IF cond-expr then-stmt else-stmt)
        (list "if" " " (gen cond-expr state) "\n"
              (gen then-stmt state)
              "else" "\n"
              (gen else-stmt state)
              "endif" "\n")]
      [#('$WHILE expr stmt)
        (list "while" " " (gen expr state) "\n"
              (gen stmt state)
              "endwhile" "\n")]
      [#('$FOR lvar expr stmt)
        (list "for"
              " "
              (lvar-new-name lvar)
              " "
              "in"
              " "
              (gen expr state)
              "\n"
              (gen stmt state)
              "endfor" "\n")]
      [#('$BREAK)
        '("break" "\n")]
      [#('$NEXT)
        '("continue" "\n")]
      [#('$RET expr)
        (list "return" " " (gen expr state) "\n")]
      [#('$FUNC func-name (arg-names ...) stmt)
        (define (rename arg-name)
          (if (eq? arg-name '...)
            "a:000"
            (string->symbol #`"a:,arg-name")))
        (define new-state
                (derive-state state
                  'in-funcp #t
                  'func-args (append
                               (map (^n (cons n (rename n))) arg-names)
                               (ref state 'func-args))))
        (list "function!"
              " "
              (convert-identifier-conventions (symbol->string func-name))
              "("
              (intersperse
                ","
                (map
                  (lambda (arg-name)
                    (convert-identifier-conventions (symbol->string arg-name)))
                  arg-names))
              ")"
              "\n"
              (gen stmt new-state)
              "endfunction" "\n")]
      [#('$FUNC~ func-name (args ...) stmt)
        (list "function!"
              " "
              (convert-identifier-conventions (symbol->string func-name))
              "("
              (intersperse "," (map lvar-arg-name args))
              ")"
              "\n"
              (gen stmt state)
              "endfunction" "\n")]
      [#('$EX (obj-or-iforms ...))
        (letrec ([zap (lambda (x)
                        (cond
                          [(iform? x)
                           (gen x state)]
                          [(pair? x)
                           (cons (zap (car x))
                                 (zap (cdr x)))]
                          [else
                            x]))])
              (list (intersperse " " (map zap obj-or-iforms))
                    "\n"))]
      [else
        (errorf "This iform is not valid: ~s" iform)])))




;;; __END__
