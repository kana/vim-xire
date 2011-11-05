#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use text.tree)
(use util.list)
(use util.match)
(use vim.xire)




(describe "xire-translate"
  (define env (make <xire-env>))
  (define toplevel-ctx (make-toplevel-ctx))
  (define stmt-ctx (make-stmt-ctx toplevel-ctx))
  (define expr-ctx (make-expr-ctx toplevel-ctx))
  (define (translate s)
    (call-with-string-io s
      (cut xire-translate <> <> :env env)))
  (xire-register-macro!
    'macro
    (lambda (form ctx)
      (IVS (Q "<")
           (apply Q (intersperse " " form))
           (Q ">")))
    'stmt
    env)
  (it "should output nothing if given script is empty"
    (expect (translate "") equal? "")
    (expect (translate "   ") equal? "")
    (expect (translate "; empty") equal? "")
    )
  (it "should raise error for a malformed script"
    (expect (translate "()") raise?)
    (expect (translate "invalid expression") raise?)
    (expect (translate "(no-such-macro)") raise?)
    )
  (it "should handle directive: define-xire-macro"
    (expect
      (translate "(define-xire-macro stmt (directive-macro f c) 0)")
      equal?
      "")
    (expect (xire-lookup-macro 'directive-macro stmt-ctx env) not eq? #f)
    (expect (xire-lookup-macro 'directive-macro expr-ctx env) eq? #f)
    )
  (it "should handle directive: define-xire-stmt"
    (expect
      (translate "(define-xire-stmt stmt-macro [(stmt-macro) #f])")
      equal?
      "")
    (expect (xire-lookup-macro 'stmt-macro stmt-ctx env) not eq? #f)
    (expect (xire-lookup-macro 'stmt-macro expr-ctx env) eq? #f)
    )
  (it "should handle directive: define-xire-expr"
    (expect
      (translate "(define-xire-expr expr-macro [(expr-macro) #f])")
      equal?
      "")
    (expect (xire-lookup-macro 'expr-macro stmt-ctx env) eq? #f)
    (expect (xire-lookup-macro 'expr-macro expr-ctx env) not eq? #f)
    )
  (it "should handle directive: scheme"
    (define output
      (with-output-to-string
        (lambda ()
          (expect (translate "(scheme (write 123) (write 456))") equal? ""))))
    (expect output equal? "123456")
    (let ([x 1])
      (expect (translate "(scheme (define x 2) (set! x 3))") equal? "")
      (expect x equal? 1)
      )
    )
  (it "should handle use of Xire macro"
    (expect (translate "(macro)") equal? "<macro>")
    (expect (translate "(macro use)") equal? "<macro use>")
    (expect (translate "(macro use test)") equal? "<macro use test>")
    )
  (it "should handle a compiled form"
    SKIP "There is no readable external representation of compiled forms."
    )
  )

(describe "xire-compile"
  (define env (make <xire-env>))
  (define (compile form ctx)
    (parameterize ([xire-env env])
      (with-output-to-string
        (lambda ()
          (write-tree (xire-compile form ctx))))))
  (define toplevel-ctx (make-toplevel-ctx))
  (define stmt-ctx (make-stmt-ctx toplevel-ctx))
  (define expr-ctx (make-expr-ctx toplevel-ctx))
  (xire-register-macro!
    'macro
    (lambda (form ctx)
      (match form
        [(m xs)
         `(,m ,xs ())]
        [(m () ys)
         (IVS (apply Q `("<" ,@(intersperse " " ys) ">")))]
        [(m (x . xs) ys)
         `(,m ,xs ,(cons x ys))]))
    'stmt
    env)
  (it "should compile Xire macros recursively"
    (expect (compile '(macro ()) stmt-ctx) equal? "<>")
    (expect (compile '(macro (x)) stmt-ctx) equal? "<x>")
    (expect (compile '(macro (x y)) stmt-ctx) equal? "<y x>")
    (expect (compile '(macro (x y z)) stmt-ctx) equal? "<z y x>")
    )
  (it "should compile use of undefined macro in expr-ctx as function call"
    (expect (compile '(MyFunc) expr-ctx) equal? "MyFunc()")
    (expect (compile '(MyFunc 1 2 3) expr-ctx) equal? "MyFunc(1,2,3)")
    )
  (it "should return form as is if it is already compiled"
    (define compiled-form (IVS (Q 1 2 3)))
    (expect (xire-compile compiled-form expr-ctx) eq? compiled-form)
    )
  (it "should compile a Scheme object in expression context"
    (expect (compile #f expr-ctx) equal? "0")
    (expect (compile #f stmt-ctx) raise?)
    ; See also: scheme-object->vim-script-notation
    )
  )

(describe "xire-compile-expr"
  (define env (make <xire-env>))
  (define (compile form ctx)
    (parameterize ([xire-env env])
      (with-output-to-string
        (lambda ()
          (write-tree (xire-compile-expr form ctx))))))
  (define toplevel-ctx (make-toplevel-ctx))
  (define stmt-ctx (make-stmt-ctx toplevel-ctx))
  (define expr-ctx (make-expr-ctx toplevel-ctx))
  (it "should compile an expression in expression context"
    (expect (compile 'foo expr-ctx) equal? "foo")
    )
  (it "should compile an expression even if a statement context is given"
    (expect (compile 'foo stmt-ctx) equal? "foo")
    )
  )

(describe "xire-compile-forms"
  (define env (make <xire-env>))
  (define (compile forms ctx)
    (parameterize ([xire-env env])
      (with-output-to-string
        (lambda ()
          (write-tree (xire-compile-forms forms ctx))))))
  (define toplevel-ctx (make-toplevel-ctx))
  (define stmt-ctx (make-stmt-ctx toplevel-ctx))
  (define expr-ctx (make-expr-ctx toplevel-ctx))
  (xire-register-macro!
    'macro
    (lambda (form ctx)
      (IVS (apply Q `("<" ,@(intersperse " " form) ">"))))
    'stmt
    env)
  (it "should compile list of forms in expression context"
    (expect (compile '(1 2 3) expr-ctx) equal? "123")
    )
  (it "should compile list of forms in statement context"
    (expect (compile '((macro 1) (macro 2) (macro 3)) stmt-ctx)
            equal?
            "<macro 1><macro 2><macro 3>")
    )
  )

(describe "rename-local-bindings"
  (define (check value ctx)
    (expect (rename-local-bindings value ctx) eq? value))
  (it "should leave form as is if it is not a variable reference"
    (define ctx (make-func-ctx (make-toplevel-ctx) '(a b c)))
    (check #f ctx)
    (check #t ctx)
    (check 123 ctx)
    (check "foo" ctx)
    (check #/bar/ ctx)
    (check '(func arg) ctx)
    )
  (it "should leave form as is if it is not in a function context (1)"
    (define ctx (make-toplevel-ctx))
    (check 'a ctx)
    (check 'b ctx)
    (check 'c ctx)
    (check 'd ctx)
    )
  (it "should leave form as is if it is not in a function context (2)"
    (define ctx (make-local-ctx (make-toplevel-ctx) '(x y z)))
    (check 'x ctx)
    (check 'y ctx)
    (check 'z ctx)
    (check 'g ctx)
    )
  (it "should rename form if it is a reference to a function parameter"
    (define ctx (make-func-ctx (make-toplevel-ctx) '(a b c ...)))
    (expect (rename-local-bindings 'a ctx) eq? 'a:a)
    (expect (rename-local-bindings 'b ctx) eq? 'a:b)
    (expect (rename-local-bindings 'c ctx) eq? 'a:c)
    (expect (rename-local-bindings '... ctx) eq? 'a:000)
    (expect (rename-local-bindings 'd ctx) eq? 'd)
    )
  (it "should rename form if it is a reference to a function local variable"
    (define ctx (make-local-ctx (make-func-ctx (make-toplevel-ctx) '(a b c))
                                '(x y z)))
    (expect (rename-local-bindings 'a ctx) eq? 'a:a)
    (expect (rename-local-bindings 'b ctx) eq? 'a:b)
    (expect (rename-local-bindings 'c ctx) eq? 'a:c)
    (expect (rename-local-bindings 'x ctx) not eq? 'x)
    (expect (rename-local-bindings 'y ctx) not eq? 'y)
    (expect (rename-local-bindings 'z ctx) not eq? 'z)
    (expect (rename-local-bindings 'g ctx) eq? 'g)
    )
  (it "should rename a local variable from outer context if necessary"
    ; (define (_)
    ;   ; ctx0
    ;   (let ([x 3]
    ;         [y 2])
    ;     ; ctx1
    ;     (let ([x (* x y)])
    ;       ; ctx2
    ;       ...)))
    (define ctx0 (make-func-ctx (make-toplevel-ctx) '()))
    (define ctx1 (make-local-ctx ctx0 '(x y)))
    (define ctx2 (make-local-ctx ctx1 '(x)))
    (expect (rename-local-bindings 'x ctx0) eq? 'x)
    (expect (rename-local-bindings 'y ctx0) eq? 'y)
    (expect (rename-local-bindings 'x ctx1) not eq? 'x)
    (expect (rename-local-bindings 'y ctx1) not eq? 'y)
    (expect (rename-local-bindings 'x ctx2) not eq? 'x)
    (expect (rename-local-bindings 'y ctx2) not eq? 'y)
    (expect (rename-local-bindings 'x ctx1)
            not eq? (rename-local-bindings 'x ctx2))
    (expect (rename-local-bindings 'y ctx1)
            eq? (rename-local-bindings 'y ctx2))
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
