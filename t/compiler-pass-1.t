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
  )




(run-suites)

; __END__
; vim: filetype=scheme
