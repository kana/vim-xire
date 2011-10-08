#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use text.tree)
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

(describe "define-xire-macro"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (it "should define new macro for given context in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) eq? #f)
      (define-xire-macro stmt (foo form ctx)
        "stmt1"
        "stmt2")
      (define-xire-macro expr (foo form ctx)
        "expr1"
        "expr2")
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) procedure?)
      )
    )
  )

(describe "define-xire-expr :low"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (define (compile form ctx)
    (with-output-to-string
      (lambda ()
        (write-tree (xire-compile form ctx)))))
  (it "should define new expression macro in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) eq? #f)
      (define-xire-expr foo :low
        [(_ 1)
         '(foo 2)]
        [(_ x)
         `("" ,x ,x)])
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) procedure?)
      (expect (compile '(foo 1) stmt-ctx) raise?)
      (expect (compile '(foo 2) stmt-ctx) raise?)
      (expect (compile '(foo 3) stmt-ctx) raise?)
      (expect (compile '(foo 1) expr-ctx) equal? "22")
      (expect (compile '(foo 2) expr-ctx) equal? "22")
      (expect (compile '(foo 3) expr-ctx) equal? "33")
      )
    )
  )

(describe "define-xire-stmt :low"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (define (compile form ctx)
    (with-output-to-string
      (lambda ()
        (write-tree (xire-compile form ctx)))))
  (it "should define new statement macro in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) eq? #f)
      (define-xire-stmt foo :low
        [(_ 1)
         '(foo 2)]
        [(_ x)
         `("" ,x ,x)])
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) eq? #f)
      (expect (compile '(foo 1) stmt-ctx) equal? "22")
      (expect (compile '(foo 2) stmt-ctx) equal? "22")
      (expect (compile '(foo 3) stmt-ctx) equal? "33")
      (expect (compile '(foo 1) expr-ctx) equal? "(foo(1))")
      (expect (compile '(foo 2) expr-ctx) equal? "(foo(2))")
      (expect (compile '(foo 3) expr-ctx) equal? "(foo(3))")
      )
    )
  )

(describe "define-xire-stmt shorthand"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (define (compile form ctx)
    (with-output-to-string
      (lambda ()
        (write-tree (xire-compile form ctx)))))
  (it "should define a statement macro for simple cocmmand"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'break stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'break expr-ctx (xire-env)) eq? #f)
      (define-xire-stmt break)
      (expect (xire-lookup-macro 'break stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'break expr-ctx (xire-env)) eq? #f)
      (expect (compile '(break) stmt-ctx) equal? "break\n")
      )
    )
  (it "should define a statement macro for simple cocmmand with different name"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'halt stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'halt expr-ctx (xire-env)) eq? #f)
      (define-xire-stmt halt "break")
      (expect (xire-lookup-macro 'halt stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'halt expr-ctx (xire-env)) eq? #f)
      (expect (compile '(halt) stmt-ctx) equal? "break\n")
      )
    )
  (it "should define a statement macro for simple cocmmand with '!'"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'break stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'break! stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'break expr-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'break! expr-ctx (xire-env)) eq? #f)
      (define-xire-stmt break :!)
      (expect (xire-lookup-macro 'break stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'break! stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'break expr-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'break! expr-ctx (xire-env)) eq? #f)
      (expect (compile '(break) stmt-ctx) equal? "break\n")
      (expect (compile '(break!) stmt-ctx) equal? "break!\n")
      )
    )
  )




(run-suites)

; vim: filetype=scheme
