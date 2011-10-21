#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use vim.xire)




(describe "=ex="
  (it "should insert an empty list into the first element of new list"
    (expect (=ex= 1 2 3) equal? '(() 1 "\n" 2 "\n" 3 "\n"))
    )
  (it "should insert newline after each element"
    (expect (=ex= 1 2 3) equal? '(() 1 "\n" 2 "\n" 3 "\n"))
    )
  (it "should insert spaces between values in list elements"
    (expect (=ex= '(if (foo == bar))
                  '(echo 1 2)
                  'endif)
            equal?
            '(()
              (if " " (foo == bar)) "\n"
              (echo " " 1 " " 2) "\n"
              endif "\n"))
    )
  (it "should leave already processed elements as is"
    (expect (=ex= '(if (foo == bar))
                  (=ex= '(echo 1 2))
                  'endif)
            equal?
            `(()
              (if " " (foo == bar)) "\n"
              ,@(=ex= '(echo 1 2))
              endif "\n"))
    )
  )

(describe "generate-match-body"
  (it "should generate body with 0 slots"
    (expect (generate-match-body '(syntax clear)
                                 '(`(=ex= (syntax clear))
                                   `(=ex= (echo "..."))))
            equal?
            '(let ()
               `(=ex= (syntax clear))
               `(=ex= (echo "..."))))
    )
  (it "should generate body with 1 or more slots"
    (expect (generate-match-body '(if $cond:expr $then:stmt)
                                 '(`(=ex= (if ,$cond)
                                          ,$then
                                          endif)))
            equal?
            '(let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                   [$then (transform-value $then:stmt #f 'stmt ctx)])
               `(=ex= (if ,$cond)
                      ,$then
                      endif)))
    )
  (it "should generate body with 1 or more ellipses"
    (expect (generate-match-body '(when $cond:expr $then:stmt ...)
                                 '(`(=ex= (if ,$cond)
                                          ,@$then
                                          endif)))
            equal?
            '(let ([$cond (transform-value $cond:expr #f 'expr ctx)]
                   [$then (transform-value $then:stmt #t 'stmt ctx)])
               `(=ex= (if ,$cond)
                      ,@$then
                      endif)))
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
  )

(describe "transform-value"
  (define stmt-ctx (make-stmt-ctx (make-toplevel-ctx)))
  (define expr-ctx (make-expr-ctx (make-toplevel-ctx)))
  (it "should convert given form into equivalent one in Vim script"
    (parameterize ([xire-env (make <xire-env>)])
      (define-xire-stmt halt "break")
      (define-xire-stmt quit "quit")
      (expect (transform-value '(halt) #f 'stmt stmt-ctx)
              equal? (=ex= "break"))
      (expect (transform-value '(halt) #f 'expr expr-ctx)
              equal? (xire-compile '(halt) expr-ctx))
      (expect (transform-value '((halt) (quit)) #t 'stmt stmt-ctx)
              equal? (list (=ex= "break") (=ex= "quit")))
      (expect (transform-value '((halt) (quit)) #t 'expr expr-ctx)
              equal? (list (xire-compile '(halt) expr-ctx)
                           (xire-compile '(quit) expr-ctx)))
      (expect (transform-value '(halt) #f 'stmt expr-ctx)
              equal? (=ex= "break"))
      (expect (transform-value '(halt) #f 'expr stmt-ctx)
              equal? (xire-compile '(halt) expr-ctx))
      )
    )
  )




(run-suites)

; vim: filetype=scheme
