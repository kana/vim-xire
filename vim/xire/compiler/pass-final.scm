(define-module vim.xire.compiler.pass-final
  (export
    ; Public API.
    pass-final

    ; Not public, but exported to test.
    bin-op-table
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




;;; Entry point
;;; ===========

(define (pass-final iforms)
  (map pass-final/rec iforms))

(define (pass-final/rec iform)
  (let gen ([iform iform])
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
        (list (gen func-expr)
              "("
              (intersperse "," (map gen arg-exprs))
              ")")]
      [#('$CALL 'if (cond-expr then-expr else-expr))
        (list "("
              (gen cond-expr)
              " ? "
              (gen then-expr)
              " : "  ; To parse r?s:t as (r)?(s):(t) not (r)?(s:t).
              (gen else-expr)
              ")")]
      [#('$CALL (= bin-op-info op) (left-expr right-expr))
        (=> next)
        (unless op
          (next))
        (list "("
              (gen left-expr)
              " "
              (cdr op)
              " "
              (gen right-expr)
              ")")]
      [#('$CALL (= un-op-info op) (expr))
        (=> next)
        (unless op
          (next))
        (list "("
              (cdr op)
              (gen expr)
              ")")]
      [#('$CALL 'ref (collection-expr index-expr))
        (list "("
              (gen collection-expr)
              "["
              (gen index-expr)
              "]"
              ")")]
      [#('$CALL 'slice (collection-expr from-expr until-expr))
        (list "("
              (gen collection-expr)
              "["
              (if from-expr
                (gen from-expr)
                '())
              " : "  ; To parse l[s:x] as l[(s):x] not l[(s:x)].
              (if until-expr
                (gen until-expr)
                '())
              "]"
              ")")]
      [#('$CALL '-> (dict-expr name))
        (list "("
              (gen dict-expr)
              "."
              name
              ")")]
      [#('$CALL 'kbd (expr))
        (convert-key-sequence-conventions expr)]
      [#('$CALL 'list (exprs ...))
        (list "["
              (intersperse "," (map gen exprs))
              "]")]
      [#('$CALL 'dict ((key-exprs ...) (val-exprs ...)))
        (list "{"
              (intersperse
                ","
                (map (lambda (key-expr val-expr)
                       (list (gen key-expr)
                             " : "  ; To parse {s:x} as {(s):x} not {(s:x)}.
                             (gen val-expr)))
                     key-exprs
                     val-exprs))
              "}")]
      [#((or '$DEF '$GSET) gvar expr)
        (list "let"
              " "
              (convert-identifier-conventions (symbol->string gvar))
              "="
              (gen expr)
              "\n")]
      [#('$LSET lvar expr)
        (list "let"
              " "
              (lvar-new-name lvar)
              "="
              (gen expr)
              "\n")]
      [#('$LET (lvars ...) stmt)
        (list (map (lambda (lvar)
                     (list "let"
                           " "
                           (lvar-new-name lvar)
                           "="
                           (gen (lvar-init-expr lvar))
                           "\n"))
                   lvars)
              (gen stmt))]
      [#('$BEGIN (stmts ...))
        (map gen stmts)]
      [#('$IF cond-expr then-stmt else-stmt)
        (list "if" " " (gen cond-expr) "\n"
              (gen then-stmt)
              "else" "\n"
              (gen else-stmt)
              "endif" "\n")]
      [#('$WHILE expr stmt)
        (list "while" " " (gen expr) "\n"
              (gen stmt)
              "endwhile" "\n")]
      [#('$FOR lvar expr stmt)
        (list "for"
              " "
              (lvar-new-name lvar)
              " "
              "in"
              " "
              (gen expr)
              "\n"
              (gen stmt)
              "endfor" "\n")]
      [#('$BREAK)
        '("break" "\n")]
      [#('$NEXT)
        '("continue" "\n")]
      [#('$RET expr)
        (list "return" " " (gen expr) "\n")]
      [#('$FUNC func-name (args ...) stmt)
        (list "function!"
              " "
              (convert-identifier-conventions (symbol->string func-name))
              "("
              (intersperse "," (map lvar-arg-name args))
              ")"
              "\n"
              (gen stmt)
              "endfunction" "\n")]
      [#('$EX (obj-or-iforms ...))
        (letrec ([zap (lambda (x)
                        (cond
                          [(iform? x)
                           (gen x)]
                          [(pair? x)
                           (cons (zap (car x))
                                 (zap (cdr x)))]
                          [else
                            x]))])
              (list (intersperse " " (map zap obj-or-iforms))
                    "\n"))]
      [(? (^x (eq? (ref (class-of x) 'name) '<ivs>)) ivs)
        ivs]
      [else
        (errorf "This iform is not valid: ~s" iform)])))




;;; __END__
