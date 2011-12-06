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
    (expect (ensure-expr-ctx 'form (make-expr-ctx (make-root-ctx))) not raise?)
    (expect (ensure-expr-ctx 'form (make-stmt-ctx (make-root-ctx))) raise?)
    )
  )

(describe "ensure-stmt-ctx"
  (it "should raise error if non-statement context is given"
    (expect (ensure-stmt-ctx 'form (make-stmt-ctx (make-root-ctx))) not raise?)
    (expect (ensure-stmt-ctx 'form (make-expr-ctx (make-root-ctx))) raise?)
    )
  )

(describe "expr-ctx?"
  (it "should return true for expression context"
    (expect (expr-ctx? (make-expr-ctx (make-root-ctx))) eq? #t)
    (expect (expr-ctx? (make-stmt-ctx (make-root-ctx))) eq? #f)
    )
  )

(describe "func-ctx?"
  (it "should return true for function context"
    (define ctx (make-root-ctx))
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
    (expect (expr-ctx? (make-expr-ctx (make-root-ctx))) eq? #t)
    (expect (expr-ctx? (make-expr-ctx (make-stmt-ctx (make-root-ctx)))) eq? #t)
    )
  )

(describe "make-func-ctx"
  (it "should make a function context from a given context"
    (define c1 (make-stmt-ctx (make-root-ctx)))
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
    (expect (ref c2 'func-args)
            equal?
            (list (cons 'a
                        (make <lvar>
                              :src-name 'a
                              :new-name 'a:a
                              :arg-name 'a))
                  (cons 'b
                        (make <lvar>
                              :src-name 'b
                              :new-name 'a:b
                              :arg-name 'b))
                  (cons 'c
                        (make <lvar>
                              :src-name 'c
                              :new-name 'a:c
                              :arg-name 'c))))
    (check c3 c2)
    (expect (ref c2 'in-funcp) eq? (ref c3 'in-funcp))
    (expect (ref c2 'func-args) equal? (ref c3 'func-args))
    )
  )

(describe "make-local-ctx"
  (it "should make a local binding context from a given context"
    (define c1 (make-stmt-ctx (make-root-ctx)))
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
    (expect (make-local-ctx (make-root-ctx :in-scriptp #f) '(x))
            raise? <error>)
    )
  )

(describe "make-root-ctx"
  (it "should make a root context"
    (define ctx (make-root-ctx))
    (expect (stmt-ctx? ctx) eq? #t)
    (expect (expr-ctx? ctx) eq? #f)
    (expect (func-ctx? ctx) eq? #f)
    (expect (script-ctx? ctx) eq? #t)
    (expect (ref ctx 'func-args) equal? '())
    (expect (ref ctx 'locals) equal? '())
    )
  (it "should make a non-script root context"
    (define ctx (make-root-ctx :in-scriptp #f))
    (expect (stmt-ctx? ctx) eq? #t)
    (expect (expr-ctx? ctx) eq? #f)
    (expect (func-ctx? ctx) eq? #f)
    (expect (script-ctx? ctx) eq? #f)
    (expect (ref ctx 'func-args) equal? '())
    (expect (ref ctx 'locals) equal? '())
    )
  )

(describe "make-stmt-ctx"
  (it "should make a statement context from a given context"
    (expect (stmt-ctx? (make-stmt-ctx (make-root-ctx))) eq? #t)
    (expect (stmt-ctx? (make-stmt-ctx (make-expr-ctx (make-root-ctx)))) eq? #t)
    )
  )

(describe "script-ctx?"
  (it "should return true for script context"
    (define ctx1 (make-root-ctx))
    (define ctx2 (make-root-ctx :in-scriptp #f))
    (expect (script-ctx? ctx1) eq? #t)
    (expect (script-ctx? (make-stmt-ctx ctx1)) eq? #t)
    (expect (script-ctx? (make-expr-ctx ctx1)) eq? #t)
    (expect (script-ctx? (make-func-ctx ctx1 '())) eq? #t)
    (expect (script-ctx? ctx2) eq? #f)
    (expect (script-ctx? (make-stmt-ctx ctx2)) eq? #f)
    (expect (script-ctx? (make-expr-ctx ctx2)) eq? #f)
    (expect (script-ctx? (make-func-ctx ctx2 '())) eq? #f)
    )
  )

(describe "stmt-ctx?"
  (it "should return true for statement context"
    (expect (stmt-ctx? (make-stmt-ctx (make-root-ctx))) eq? #t)
    (expect (stmt-ctx? (make-expr-ctx (make-root-ctx))) eq? #f)
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
