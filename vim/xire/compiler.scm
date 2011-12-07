(define-module vim.xire.compiler
  (export
    ; Public API
    transform-value
    xire-translate

    ; Semi-public API for advanced usage.
    xire-compile
    xire-compile-expr
    xire-compile-forms

    ; Not public, but exported to test.

    ; Not public, but exported to avoid some problems.
    match  ; See [MATCHQ].
    ))
(select-module vim.xire.compiler)

(use gauche.parameter)
(use text.tree)
(use util.list)
(use util.match)
(use vim.xire.compiler.pass-1)
(use vim.xire.compiler.pass-final)
(use vim.xire.iform)
(use vim.xire.util)




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
  (define ctx (make-root-ctx))
  (define (optimize iform)
    ; Currently there is no optimization.
    ; More passes will be added in future.
    iform)

  (parameterize ([xire-env env])
    (let loop ()
      (match (read input-port)
        [(? eof-object?)
         (finish)]
        [(and ((or 'defexpr
                   'defmacro
                   'defstmt)
               . _)
              form)
         (eval form scheme-env)
         (loop)]
        [('scheme . scheme-exprs)
         (eval `(begin ,@scheme-exprs) scheme-env)
         (loop)]
        [form
         (push! compiled-vim-script-tree
                (pass-final (optimize (xire-compile form ctx))))
         (loop)]))))

;; Compile a Xire script FORM then return a resulting Vim script in IForm.
(define (xire-compile form ctx)
  (pass-1 form ctx))

;; Compile a Xire script EXPR then return a resulting Vim script in IForm.
;; This is an abbreviated form of xire-compile for typical use.
(define (xire-compile-expr expr ctx)
  (xire-compile expr
                (if (expr-ctx? ctx)
                  ctx
                  (make-expr-ctx ctx))))

;; Compile a list of Xire script FORMS then return a resulting Vim script in
;; IForm.  This is an abbreviated form of xire-compile for typical use.
(define (xire-compile-forms forms ctx)
  (map (cut xire-compile <> ctx) forms))

(define (transform-value form-or-forms manyp type upper-ctx)
  (define (fail detail)
    (errorf "Invalid usage (~a): (transform-value ~s ~s ~s ~s)"
            detail
            form-or-forms
            manyp
            type
            upper-ctx))
  (define (transform-form form)
    (cond
      [(eq? type 'expr)
       (xire-compile-expr form upper-ctx)]
      [(or (eq? type 'form)
           (eq? type 'qexpr)
           (eq? type 'qstmt))
       form]
      [(eq? type 'qsym)
       (when (not (symbol? form))
         (fail "invalid form for this type"))
       form]
      [(eq? type 'stmt)
       (xire-compile form
                     (if (stmt-ctx? upper-ctx)
                       upper-ctx
                       (make-stmt-ctx upper-ctx)))]
      [(eq? type 'sym)
       (when (not (symbol? form))
         (fail "invalid form for this type"))
       (xire-compile-expr form upper-ctx)]
      [else
        (fail "invalid type")]))
  (if manyp
    (map transform-form form-or-forms)
    (transform-form form-or-forms)))




;;; Magic
;;; =====

;; Code which defines high-level Xire macro has to bind MATCH to the macro
;; defined in util.match, because the syntax to define high-level Xire macro
;; is implemented with DEFINE-MACRO (built-in) and MATCH (util.match).  This
;; restriction is a bit annoying for development.
;;
;; [MATCHQ] NB: To avoid writing extra (use util.match) in user code,
;; MATCH (vim.xire.compiler) is bound to the same macro as MATCH (util.match),
;; and MATCH (vim.xire.compiler) is exported.
(define match match)




;;; __END__
