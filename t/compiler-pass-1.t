#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.compiler.pass-1)
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
  (it "should reject invalid forms"
    (expect (pass-1 <regexp> root-ctx)
            raise-error-like? (format "Invalid Xire form: ~s" <regexp>))
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
