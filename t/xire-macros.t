#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use vim.xire)




(describe "ctx-type->xire-env-slot-name"
  (it "shouold return proper values for valid arguments"
    (expect (ctx-type->xire-env-slot-name 'stmt) eq? 'stmt-macros)
    (expect (ctx-type->xire-env-slot-name 'expr) eq? 'expr-macros)
    )
  (it "should raise error for invalid arguments"
    (expect (ctx-type->xire-env-slot-name 'toplevel) raise?)
    (expect (ctx-type->xire-env-slot-name 'function) raise?)
    )
  )

(describe "xire-lookup-macro"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (define env (make <xire-env>))
  (define (stmt-expander form ctx) "stmt")
  (define (expr-expander form ctx) "expr")
  (hash-table-put! (ref env 'stmt-macros) 'foo stmt-expander)
  (hash-table-put! (ref env 'expr-macros) 'foo expr-expander)
  (it "should return macro expander if the macro is available"
    (expect (xire-lookup-macro 'foo stmt-ctx env) eq? stmt-expander)
    (expect (xire-lookup-macro 'foo expr-ctx env) eq? expr-expander)
    )
  (it "should return #f if the macro is not available"
    (expect (xire-lookup-macro 'bar stmt-ctx env) eq? #f)
    (expect (xire-lookup-macro 'bar expr-ctx env) eq? #f)
    )
  )

(describe "xire-register-macro!"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (define env (make <xire-env>))
  (define (stmt-expander form ctx) "stmt")
  (define (expr-expander form ctx) "expr")
  (xire-register-macro! 'foo stmt-expander 'stmt env)
  (xire-register-macro! 'foo expr-expander 'expr env)
  (it "should return macro expander if the macro is available"
    (expect (xire-lookup-macro 'foo stmt-ctx env) eq? stmt-expander)
    (expect (xire-lookup-macro 'foo expr-ctx env) eq? expr-expander)
    )
  (it "should return #f if the macro is not available"
    (expect (xire-lookup-macro 'bar stmt-ctx env) eq? #f)
    (expect (xire-lookup-macro 'bar expr-ctx env) eq? #f)
    )
  )




(run-suites)

; vim: filetype=scheme