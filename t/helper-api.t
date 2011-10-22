#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use vim.xire)




(describe "generate-match-body"
  (it "should generate body with 0 slots"
    (expect (generate-match-body '(syntax clear)
                                 '((IVS (S 'syntax 'clear))
                                   (IVS (S 'echo "..."))))
            equal?
            '(let ()
               (IVS (S 'syntax 'clear))
               (IVS (S 'echo "..."))))
    )
  (it "should generate body with 1 or more slots"
    (expect (generate-match-body '(if $cond:expr $then:stmt)
                                 '((IVS (S 'if $cond)
                                        $then
                                        (S 'endif))))
            equal?
            '(let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                   [$then (transform-value $then:stmt #f 'stmt ctx)])
               (IVS (S 'if $cond)
                    $then
                    (S 'endif))))
    )
  (it "should generate body with 1 or more ellipses"
    (expect (generate-match-body '(when $cond:expr $then:stmt ...)
                                 '((IVS (S 'if $cond)
                                        (apply IVS $then)
                                        (S 'endif))))
            equal?
            '(let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                   [$then (transform-value $then:stmt #t 'stmt ctx)])
               (IVS (S 'if $cond)
                    (apply IVS $then)
                    (S 'endif))))
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

(describe "transform-value"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (it "should transform give value into equivalent 'stmt'"
    (parameterize ([xire-env (make <xire-env>)])
      (define-xire-stmt halt break)
      (define-xire-stmt quit quit)
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
      (define-xire-stmt halt break)
      (define-xire-stmt quit quit)
      (expect (transform-value '(halt) #f 'expr expr-ctx)
              equal? (xire-compile '(halt) expr-ctx))
      (expect (transform-value '((halt) (quit)) #t 'expr expr-ctx)
              equal? (list (xire-compile '(halt) expr-ctx)
                           (xire-compile '(quit) expr-ctx)))
      (expect (transform-value '(halt) #f 'expr stmt-ctx)
              equal? (xire-compile '(halt) (make-expr-ctx stmt-ctx)))
      )
    )
  )




(run-suites)

; vim: filetype=scheme
