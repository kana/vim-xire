#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use vim.xire)




(describe "<xire-ctx>"
  (it "should be a class for a context"
    (expect (class-of <xire-ctx>) eq? <class>)
    )
  )

(describe "copy-ctx"
  (it "should copy a given context"
    (define c1 (make <xire-ctx>))
    (define c2 (copy-ctx c1))
    (for-each
      (lambda (slot-name)
        (expect (ref c2 slot-name) eq? (ref c1 slot-name)))
      (map slot-definition-name (class-direct-slots <xire-ctx>)))
    )
  )

(describe "ensure-expr-ctx"
  (it "should raise error if non-expression context is given"
    (expect (ensure-expr-ctx 'form (make <xire-ctx> :type 'expr)) not raise?)
    (expect (ensure-expr-ctx 'form (make <xire-ctx> :type 'stmt)) raise?)
    )
  )

(describe "ensure-stmt-ctx"
  (it "should raise error if non-statement context is given"
    (expect (ensure-stmt-ctx 'form (make <xire-ctx> :type 'stmt)) not raise?)
    (expect (ensure-stmt-ctx 'form (make <xire-ctx> :type 'expr)) raise?)
    )
  )

(describe "expr-ctx?"
  (it "should return true for expression context"
    (expect (expr-ctx? (make <xire-ctx> :type 'expr)) eq? #t)
    (expect (expr-ctx? (make <xire-ctx> :type 'stmt)) eq? #f)
    )
  )

(describe "make-expr-ctx"
  (it "should make an expression context from a given context"
    (define c1 (make <xire-ctx>))
    (define c2 (make-expr-ctx c1))
    (expect (expr-ctx? c2) eq? #t)
    (expect (ref c2 'toplevelp) eq? #f)
    )
  )

(describe "make-stmt-ctx"
  (it "should make a statement context from a given context"
    (define c1 (make <xire-ctx>))
    (define c2 (make-stmt-ctx c1))
    (expect (stmt-ctx? c2) eq? #t)
    (expect (ref c2 'toplevelp) eq? #f)
    )
  )

(describe "make-toplevel-ctx"
  (it "should make a top-level context"
    (define c1 (make-toplevel-ctx))
    (expect (stmt-ctx? c1) eq? #t)
    (expect (ref c1 'toplevelp) eq? #t)
    )
  )

(describe "stmt-ctx?"
  (it "should return true for statement context"
    (expect (stmt-ctx? (make <xire-ctx> :type 'stmt)) eq? #t)
    (expect (stmt-ctx? (make <xire-ctx> :type 'expr)) eq? #f)
    )
  )

(describe "toplevel-ctx?"
  (it "should return true for top-level context"
    (expect (toplevel-ctx? (make-toplevel-ctx)) eq? #t)
    (expect (toplevel-ctx? (make-stmt-ctx (make-toplevel-ctx))) eq? #f)
    (expect (toplevel-ctx? (make-expr-ctx (make-toplevel-ctx))) eq? #f)
    )
  )




(run-suites)

; vim: filetype=scheme
