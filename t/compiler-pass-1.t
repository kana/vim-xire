#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.compiler.pass-1)
(use vim.xire.iform)
(use vim.xire.util)




(define (raise-error-like? actual expected-pattern)
  (let1 message (~ actual 'original-error 'message)
    (cond
      [(string? expected-pattern)
       (string=? message expected-pattern)]
      [(regexp? expected-pattern)
       (expected-pattern message)]
      [else
        (errorf "Invalid expected-pattern: ~s" expected-pattern)])))




(describe "pass-1"
  (define root-ctx (make-root-ctx))
  (define expr-ctx (make-expr-ctx root-ctx))
  (it "should reject invalid forms"
    (expect (pass-1 <regexp> root-ctx)
            raise-error-like? (format "Invalid Xire form: ~s" <regexp>))
    )
  (it "should generate an iform from a boolean value"
    (expect (pass-1 #t expr-ctx) equal? ($const #t))
    (expect (pass-1 #f expr-ctx) equal? ($const #f))
    (expect (pass-1 #t root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" #t))
    (expect (pass-1 #f root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" #f))
    )
  (it "should generate an iform from a number"
    (expect (pass-1 123 expr-ctx) equal? ($const 123))
    (expect (pass-1 123 root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" 123))
    )
  (it "should generate an iform from a regular expression"
    (expect (pass-1 #/\<foo\>/ expr-ctx) equal? ($const #/\<foo\>/))
    (expect (pass-1 #/\<foo\>/ root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" #/\<foo\>/))
    )
  (it "should generate an iform from a string"
    (expect (pass-1 "foo" expr-ctx) equal? ($const "foo"))
    (expect (pass-1 "foo" root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" "foo"))
    )
  (it "should generate an iform from a symbol as a local variable reference"
    (define expr-ctx (make-expr-ctx root-ctx))
    (define foo1 (make <lvar>
                       :src-name 'foo1
                       :new-name (gensym)))
    (define foo2 (make <lvar>
                       :src-name 'foo2
                       :new-name (gensym)))
    (set! (ref expr-ctx 'locals)
          (list (cons 'foo foo1)
                (cons 'foo foo2)))
    (expect (pass-1 'foo expr-ctx) equal? ($lref foo1))
    (expect (pass-1 'foo root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" 'foo))
    )
  (it "should generate an iform from a symbol as a function argument reference"
    (define expr-ctx (make-expr-ctx root-ctx))
    (define foo1 (make <lvar>
                       :src-name 'foo1
                       :new-name 'a:foo1
                       :arg-name 'foo1))
    (set! (ref expr-ctx 'func-args)
          (list (cons 'foo foo1)))
    (expect (pass-1 'foo expr-ctx) equal? ($lref foo1))
    (expect (pass-1 'foo root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" 'foo))
    )
  (it "should prefer local variables to function arguments"
    (define expr-ctx (make-expr-ctx root-ctx))
    (define foo-l (make <lvar>
                        :src-name 'foo
                        :new-name (gensym)))
    (define foo-a (make <lvar>
                        :src-name 'foo
                        :new-name 'a:foo
                        :arg-name 'foo))
    (set! (ref expr-ctx 'locals) (list (cons 'foo foo-l)))
    (set! (ref expr-ctx 'func-args) (list (cons 'foo foo-a)))
    (expect (pass-1 'foo expr-ctx) equal? ($lref foo-l))
    )
  (it "should generate an iform from a symbol as a global variable reference"
    (expect (pass-1 'foo expr-ctx) equal? ($gref 'foo))
    (expect (pass-1 'foo root-ctx)
            raise-error-like?
            (format "Invalid form in an expression context: ~s" 'foo))
    )
  (it "should generate an iform for an implicit function call"
    (expect (pass-1 '(a b c) expr-ctx)
            equal? ($call ($gref 'a) (list ($gref 'b) ($gref 'c))))
    (expect (pass-1 '(a b c) root-ctx)
            raise-error-like?
            (format "Invalid Xire form: ~s" '(a b c)))
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
