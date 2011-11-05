#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use srfi-1)
(use test.gasmine)
(use vim.xire)




(describe "<xire-env>"
  (it "should be a class for an environment"
    (expect (class-of (xire-env)) eq? <xire-env>)
    )
  )

(describe "xire-env"
  (it "should return the current environment"
    (define e1 (xire-env))
    (expect (xire-env) eq? e1)
    (parameterize ([xire-env #f])
      (expect (xire-env) not eq? e1)
      (expect (xire-env) equal? #f)
      )
    (expect (xire-env) eq? e1)
    )
  )

(describe "copy-env"
  (define nothing (make <class>))
  (define (hash-table-equal? ht1 ht2)
    (every
      (lambda (key)
        (equal? (hash-table-get ht1 key nothing)
                (hash-table-get ht2 key nothing)))
      (append (hash-table-keys ht1)
              (hash-table-keys ht2))))
  (it "should copy the current environment"
    (define e1 (xire-env))
    (define e2 (copy-env e1))
    (expect e2 not eq? e1)
    (expect (ref e2 'expr-macros) hash-table-equal? (ref e1 'expr-macros))
    (expect (ref e2 'stmt-macros) hash-table-equal? (ref e1 'stmt-macros))
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
