#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use text.tree)
(use util.match)
(use vim.xire)
(use vim.xire.compiler.pass-final)




(define (compile form ctx)
  (with-output-to-string
    (lambda ()
      (write-tree (pass-final (list (xire-compile form ctx)))))))




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

(describe "defmacro"
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
  (it "should define new macro for given context in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) eq? #f)
      (defmacro stmt (foo form ctx)
        "stmt1"
        "stmt2")
      (defmacro expr (foo form ctx)
        "expr1"
        "expr2")
      (expect (xire-lookup-macro 'foo stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'foo expr-ctx (xire-env)) procedure?)
      )
    )
  )

(describe "generate-match-pat"
  (it "should escape normal symbols"
    (expect (generate-match-pat '(syntax clear))
            equal?
            '('syntax 'clear))
    )
  (it "should leave slot symbols as is"
    (expect (generate-match-pat '(if $cond:expr $then:stmt))
            equal?
            '('if $cond:expr $then:stmt))
    (expect (generate-match-pat '(when $cond:expr $then:stmt ...))
            equal?
            '('when $cond:expr $then:stmt ...))
    )
  (it "should leave non-symbol values as is"
    (expect (generate-match-pat '("just" #\a #(test)))
            equal?
            '("just" #\a #(test)))
    )
  (it "should leave _ as is"
    (expect (generate-match-pat '(_ $cond:expr $then:stmt))
            equal?
            '(_ $cond:expr $then:stmt))
    )
  )

(describe "generate-match-body"
  (it "should generate body with 0 slots"
    (expect (generate-match-body '(syntax clear)
                                 '((ex syntax clear)
                                   (ex echo "...")))
            equal?
            '(let ()
               (ex syntax clear)
               (ex echo "...")))
    )
  (it "should generate body with 1 or more slots"
    (expect (generate-match-body '(if $cond:expr $then:stmt)
                                 '(($if $cond $then ($begin '()))))
            equal?
            '(let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                   [$then (transform-value $then:stmt #f 'stmt ctx)])
               ($if $cond $then ($begin '()))))
    )
  (it "should generate body with 1 or more ellipses"
    (expect (generate-match-body '(when $cond:expr $then:stmt ...)
                                 '(($if $cond $then ($begin '()))))
            equal?
            '(let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                   [$then (transform-value $then:stmt #t 'stmt ctx)])
               ($if $cond $then ($begin '()))))
    )
  (it "should generate body with let-like pattern"
    (expect (generate-match-body '(my-let ($var:expr $value:stmt) ...)
                                 '(`(,$var ,$value)))
            equal?
            '(let ([$var (transform-value $var:expr #t 'expr ctx)]
                   [$value (transform-value $value:stmt #t 'stmt ctx)])
               `(,$var ,$value)))
    )
  )

(describe "generate-expanded-form-of-high-level-macro"
  (it "shouold generate proper form from proper input"
    (expect
      (generate-expanded-form-of-high-level-macro
        'if
        'stmt
        '[(if $cond:expr $then:stmt)
          ($if $cond $then ($begin '()))]
        '[(if $cond:expr $then:stmt $else:stmt)
          ($if $cond $then $else)])
      equal?
      '(defmacro stmt (if form ctx)
         (ensure-stmt-ctx form ctx)
         (match form
           [('if $cond:expr $then:stmt)
            (let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                  [$then (transform-value $then:stmt #f 'stmt ctx)])
              ($if $cond $then ($begin '())))]
           [('if $cond:expr $then:stmt $else:stmt)
            (let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                  [$then (transform-value $then:stmt #f 'stmt ctx)]
                  [$else (transform-value $else:stmt #f 'stmt ctx)])
              ($if $cond $then $else))])))
    )
  (it "shouold raise error for invalid context type"
    (expect
      (generate-expanded-form-of-high-level-macro
        'if
        'stmttttt
        '[(if $cond:expr $then:stmt)
          ($if $cond $then ($begin '()))]
        '[(if $cond:expr $then:stmt $else:stmt)
          ($if $cond $then $else)])
      raise?)
    )
  )

(describe "transform-value"
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
  (it "should transform give value into equivalent 'stmt'"
    (parameterize ([xire-env (make <xire-env>)])
      (defstmt halt break)
      (defstmt quit quit)
      (expect (transform-value '(halt) #f 'stmt stmt-ctx)
              equal? (xire-compile '(halt) stmt-ctx))
      (expect (transform-value '((halt) (quit)) #t 'stmt stmt-ctx)
              equal? (list (xire-compile '(halt) stmt-ctx)
                           (xire-compile '(quit) stmt-ctx)))
      (expect (transform-value '(halt) #f 'stmt expr-ctx)
              equal? (xire-compile '(halt) (make-stmt-ctx expr-ctx)))
      )
    )
  (it "should transform give value into equivalent 'expr'"
    (parameterize ([xire-env (make <xire-env>)])
      (defstmt halt break)
      (defstmt quit quit)
      (expect (transform-value '(halt) #f 'expr expr-ctx)
              equal? (xire-compile '(halt) expr-ctx))
      (expect (transform-value '((halt) (quit)) #t 'expr expr-ctx)
              equal? (list (xire-compile '(halt) expr-ctx)
                           (xire-compile '(quit) expr-ctx)))
      (expect (transform-value '(halt) #f 'expr stmt-ctx)
              equal? (xire-compile '(halt) (make-expr-ctx stmt-ctx)))
      )
    )
  (it "should transform give value into equivalent 'form'"
    (define form-a '(a "a" (a)))
    (define form-b '(b "b" (b)))
    (expect (transform-value form-a #f 'form stmt-ctx)
            eq? form-a)
    (expect (transform-value form-a #f 'form expr-ctx)
            eq? form-a)
    (expect (transform-value (list form-a form-b) #t 'form stmt-ctx)
            equal? (list form-a form-b))
    (expect (car (transform-value (list form-a form-b) #t 'form stmt-ctx))
            eq? form-a)
    (expect (cadr (transform-value (list form-a form-b) #t 'form stmt-ctx))
            eq? form-b)
    )
  (it "should transform give value into equivalent 'sym'"
    (define func-ctx (make-func-ctx (make-root-ctx) '(foo->bar)))
    (expect (transform-value 'foo->bar #f 'sym stmt-ctx)
            equal? ($gref 'foo->bar))
    (expect (transform-value 'foo->bar #f 'sym expr-ctx)
            equal? ($gref 'foo->bar))
    (expect (transform-value 'foo->bar #f 'sym func-ctx)
            equal? (xire-compile-expr 'foo->bar func-ctx))
    )
  (it "should raise error for invalid form with 'sym'"
    (expect (transform-value '(x) #f 'sym expr-ctx)
            raise? <error>)
    )
  (it "should transform give value into equivalent 'qsym'"
    (expect (transform-value 'foo #f 'qsym stmt-ctx) eq? 'foo)
    (expect (transform-value 'foo #f 'qsym expr-ctx) eq? 'foo)
    )
  (it "should raise error for invalid form with 'qsym'"
    (expect (transform-value '(foo) #f 'qsym expr-ctx) raise? <error>)
    )
  (it "should transform give value into equivalent 'qexpr'"
    (define form '(expr form))
    (expect (transform-value form #f 'qexpr expr-ctx) eq? form)
    (expect (transform-value form #f 'qexpr stmt-ctx) eq? form)
    )
  (it "should transform give value into equivalent 'qstmt"
    (define form '(stmt form))
    (expect (transform-value form #f 'qstmt expr-ctx) eq? form)
    (expect (transform-value form #f 'qstmt stmt-ctx) eq? form)
    )
  )

(describe "defexpr"
  (define stmt-ctx (make-stmt-ctx (make-root-ctx)))
  (define expr-ctx (make-expr-ctx (make-root-ctx)))
  (it "should define new expression macro in the current environment"
    (parameterize ([xire-env (make <xire-env>)])
      (expect (xire-lookup-macro if stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) eq? #f)
      (defexpr if
        [(if $cond:expr $then:expr $else:expr)
         ($const (format "~a?~a:~a" $cond $then $else))])
      (expect (xire-lookup-macro 'if stmt-ctx (xire-env)) eq? #f)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) procedure?)
      (expect (compile '(if co-nd th-en el-se) stmt-ctx) raise?)
      (expect (compile '(if co-nd th-en el-se) expr-ctx)
              equal?
              (compile ($const (format "~a?~a:~a"
                                       ($gref 'co-nd)
                                       ($gref 'th-en)
                                       ($gref 'el-se)))
                       expr-ctx))
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
         ($const (format "~a?~a" $cond $then))]
        [(if $cond:expr $then:stmt $else:stmt)
         ($const (format "~a?~a:~a" $cond $then $else))])
      (expect (xire-lookup-macro 'if stmt-ctx (xire-env)) procedure?)
      (expect (xire-lookup-macro 'if expr-ctx (xire-env)) eq? #f)
      (expect (compile '(break) stmt-ctx) equal? "break\n")
      (expect (compile '(return) stmt-ctx) equal? "return\n")
      (expect (compile '(if 3) expr-ctx) equal? "if(3)")
      (expect (compile '(if co-nd (break)) stmt-ctx)
              equal?
              (compile ($const (format "~a?~a"
                                       ($gref 'co-nd)
                                       ($ex '(break))))
                       stmt-ctx))
      (expect (compile '(if co-nd (break) (return)) stmt-ctx)
              equal?
              (compile ($const (format "~a?~a:~a"
                                       ($gref 'co-nd)
                                       ($ex '(break))
                                       ($ex '(return))))
                       stmt-ctx))
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
