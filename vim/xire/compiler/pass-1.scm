(define-module vim.xire.compiler.pass-1
  (export
    ; Public API.
    pass-1

    ; Not public, but exported to test.
    ))
(select-module vim.xire.compiler.pass-1)

(use util.list)
(use util.match)
(use vim.xire.iform)
(use vim.xire.util)




;;; Entry point
;;; ===========

;; Parse a given FORM and return a corresponding iform.
;;
;; NB: Most parts of the language such as "if" statement are implemented as
;; built-in macros.
(define (pass-1 form ctx)
  (define (report-syntax-error)
    (errorf "Invalid Xire form: ~s" form))
  (match form
    [(? boolean? b)
     (ensure-expr-ctx form ctx)
     ($const b)]
    [(? number? n)
     (ensure-expr-ctx form ctx)
     ($const n)]
    [(? regexp? r)
     (ensure-expr-ctx form ctx)
     ($const r)]
    [(? string? s)
     (ensure-expr-ctx form ctx)
     ($const s)]
    [(? symbol? name)
     (ensure-expr-ctx form ctx)
     (cond
       [(or (assq name (ref ctx 'locals))
            (assq name (ref ctx 'func-args)))
        => (.$ $lref cdr)]
       [else
         ($gref name)])]
    [_
      (report-syntax-error)]))




;;; __END__
