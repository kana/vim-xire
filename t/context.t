#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use srfi-1)
(use test.gasmine)
(use vim.xire)




(describe "<xire-ctx>"
  (it "should be a class for a context"
    (expect (class-of <xire-ctx>) eq? <class>)
    )
  )

(describe "copy-ctx"
  (it "should copy a given context"
    (define c1 (make <xire-ctx>  ; with slots having non-default values.
                     :type 'expr
                     :in-scriptp #f
                     :in-funcp #t
                     :func-args '(a)
                     :locals '(l)))
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

(describe "func-ctx?"
  (it "should return true for function context"
    (define ctx (make-toplevel-ctx))
    (expect (func-ctx? ctx) eq? #f)
    (expect (func-ctx? (make-stmt-ctx ctx)) eq? #f)
    (expect (func-ctx? (make-expr-ctx ctx)) eq? #f)
    (expect (func-ctx? (make-func-ctx ctx '())) eq? #t)
    (expect (func-ctx? (make-func-ctx (make-stmt-ctx ctx) '())) eq? #t)
    (expect (func-ctx? (make-func-ctx (make-expr-ctx ctx) '())) eq? #t)
    (expect (func-ctx? (make-expr-ctx (make-func-ctx ctx '()))) eq? #t)
    (expect (func-ctx? (make-stmt-ctx (make-func-ctx ctx '()))) eq? #t)
    (expect (func-ctx? (make-func-ctx (make-func-ctx ctx '()) '())) eq? #t)
    )
  )

(describe "make-expr-ctx"
  (it "should make an expression context from a given context"
    (define c1 (make <xire-ctx>))
    (define c2 (make-expr-ctx c1))
    (expect (expr-ctx? c2) eq? #t)
    )
  )

(describe "make-func-ctx"
  (it "should make a function context from a given context"
    (define c1 (make-stmt-ctx (make-toplevel-ctx)))
    (define c2 (make-func-ctx c1 '(a b c)))
    (define c3 (make-stmt-ctx c2))
    (define (check %c1 %c2)
      (define different-slot-names '(in-funcp func-args))
      (for-each
        (lambda (slot-name)
          (expect (ref %c2 slot-name) equal? (ref %c1 slot-name)))
        (filter
          (lambda (slot-name)
            (not (memq slot-name different-slot-names)))
          (map slot-definition-name (class-direct-slots <xire-ctx>)))))
    (check c2 c1)
    (expect (ref c1 'in-funcp) eq? #f)
    (expect (ref c2 'in-funcp) eq? #t)
    (expect (ref c1 'func-args) equal? '())
    (expect (ref c2 'func-args) equal? '(a b c))
    (check c3 c2)
    (expect (ref c2 'in-funcp) eq? (ref c3 'in-funcp))
    (expect (ref c2 'func-args) equal? (ref c3 'func-args))
    )
  )

(describe "make-local-ctx"
  (it "should make a local binding context from a given context"
    (define c1 (make-stmt-ctx (make-toplevel-ctx)))
    (define c2 (make-local-ctx c1 '(a b c)))
    (define c3 (make-stmt-ctx c2))
    (define (check %c1 %c2)
      (define different-slot-names '(locals))
      (for-each
        (lambda (slot-name)
          (expect (ref %c2 slot-name) equal? (ref %c1 slot-name)))
        (filter
          (lambda (slot-name)
            (not (memq slot-name different-slot-names)))
          (map slot-definition-name (class-direct-slots <xire-ctx>)))))
    (check c2 c1)
    (expect (ref c1 'locals) equal? '())
    (expect (map (lambda (p) (cons (car p) '_)) (ref c2 'locals))
            equal? '((a . _) (b . _) (c . _)))
    (check c3 c2)
    (expect (ref c2 'locals) equal? (ref c3 'locals))
    )
  (it "should fail to 'inherit' from non-function and non-script context"
    (expect (make-local-ctx (make <xire-ctx> :in-scriptp #f) '(x))
            raise? <error>)
    )
  )

(describe "make-stmt-ctx"
  (it "should make a statement context from a given context"
    (define c1 (make <xire-ctx>))
    (define c2 (make-stmt-ctx c1))
    (expect (stmt-ctx? c2) eq? #t)
    )
  )

(describe "make-toplevel-ctx"
  (it "should make a top-level context"
    (define c1 (make-toplevel-ctx))
    (expect (stmt-ctx? c1) eq? #t)
    )
  )

(describe "script-ctx?"
  (it "should return true for script context"
    (define ctx (make-toplevel-ctx))
    (expect (script-ctx? ctx) eq? #t)
    (expect (script-ctx? (make-stmt-ctx ctx)) eq? #t)
    (expect (script-ctx? (make-expr-ctx ctx)) eq? #t)
    (expect (script-ctx? (make-func-ctx ctx '())) eq? #t)
    (expect (script-ctx? (make <xire-ctx>)) eq? #t)
    (expect (script-ctx? (make <xire-ctx> :in-scriptp #f)) eq? #f)
    )
  )

(describe "stmt-ctx?"
  (it "should return true for statement context"
    (expect (stmt-ctx? (make <xire-ctx> :type 'stmt)) eq? #t)
    (expect (stmt-ctx? (make <xire-ctx> :type 'expr)) eq? #f)
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
