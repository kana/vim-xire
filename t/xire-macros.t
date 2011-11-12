#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use text.tree)
(use util.match)
(use vim.xire)




(define (compile form ctx)
  (with-output-to-string
    (lambda ()
      (write-tree (xire-compile form ctx)))))




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
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
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
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
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
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
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

(describe "generate-expanded-form-of-high-level-macro"
  (it "shouold generate proper form from proper input"
    (expect
      (generate-expanded-form-of-high-level-macro
        'if
        'stmt
        '[(if $cond:expr $then:stmt)
          (IVS (S 'if $cond)
               $then
               (S 'endif))]
        '[(if $cond:expr $then:stmt $else:stmt)
          (IVS (S 'if $cond)
               $then
               (S 'else)
               $else
               (S 'endif))])
      equal?
      '(define-xire-macro stmt (if form ctx)
         (ensure-stmt-ctx form ctx)
         (match form
           [('if $cond:expr $then:stmt)
            (let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                  [$then (transform-value $then:stmt #f 'stmt ctx)])
              (IVS (S 'if $cond)
                   $then
                   (S 'endif)))]
           [('if $cond:expr $then:stmt $else:stmt)
            (let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                  [$then (transform-value $then:stmt #f 'stmt ctx)]
                  [$else (transform-value $else:stmt #f 'stmt ctx)])
              (IVS (S 'if $cond)
                   $then
                   (S 'else)
                   $else
                   (S 'endif)))])))
    )
  (it "shouold raise error for invalid context type"
    (expect
      (generate-expanded-form-of-high-level-macro
        'if
        'stmttttt
        '[(if $cond:expr $then:stmt)
          (IVS (S 'if $cond)
               $then
               (S 'endif))]
        '[(if $cond:expr $then:stmt $else:stmt)
          (IVS (S 'if $cond)
               $then
               (S 'else)
               $else
               (S 'endif))])
      raise?)
    )
  )

(describe "define-xire-expr"
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
  (it "should define new expression macro in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro if stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) eq? #f)
      (define-xire-expr if
        [(if $cond:expr $then:expr $else:expr)
         (IVS $cond (Q "?") $then (Q ":") $else)])
      (expect (xire-lookup-macro 'if stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) procedure?)
      (expect (compile '(if co-nd th-en el-se) stmt-ctx) raise?)
      (expect (compile '(if co-nd th-en el-se) expr-ctx)
              equal?
              "co_nd?th_en:el_se")
      )
    )
  )

(describe "defstmt"
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
  (define (lines . strings)
    (string-join strings "\n" 'suffix))
  (it "should define new statement macro in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'if stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) eq? #f)
      (defstmt break)
      (defstmt return)
      (defstmt if
        [(if $cond:expr $then:stmt)
         (IVS (S 'if $cond)
              $then
              (S 'endif))]
        [(if $cond:expr $then:stmt $else:stmt)
         (IVS (S 'if $cond)
              $then
              (S 'else)
              $else
              (S 'endif))])
      (expect (xire-lookup-macro 'if stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) eq? #f)
      (expect (compile '(break) stmt-ctx) equal? "break\n")
      (expect (compile '(return) stmt-ctx) equal? "return\n")
      (expect (compile '(if 3) expr-ctx) equal? "if(3)")
      (expect (compile '(if co-nd (break)) stmt-ctx)
              equal?
              (lines "if co_nd"
                     "break"
                     "endif"))
      (expect (compile '(if co-nd (break) (return)) stmt-ctx)
              equal?
              (lines "if co_nd"
                     "break"
                     "else"
                     "return"
                     "endif"))
      )
    )
  )

(describe "defstmt shorthand"
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
  (it "should define a statement macro for simple cocmmand"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'break stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'break expr-ctx (xire-env)) eq? #f)
      (defstmt break)
      (expect (xire-lookup-macro 'break stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'break expr-ctx (xire-env)) eq? #f)
      (expect (compile '(break) stmt-ctx) equal? "break\n")
      )
    )
  (it "should define a statement macro for simple cocmmand with different name"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'halt stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'halt expr-ctx (xire-env)) eq? #f)
      (defstmt halt break)
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
      (defstmt break :!)
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

; __END__
; vim: filetype=scheme
