#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use gauche.parameter)
(use test.gasmine)
(use vim.xire)




(describe "convert-identifier-conventions"
  (it "should return the same spell if ordinary identifier is given"
    (expect (convert-identifier-conventions "foo") equal? "foo")
    (expect (convert-identifier-conventions "bar123") equal? "bar123")
    (expect (convert-identifier-conventions "s:local") equal? "s:local")
    )
  (it "should convert 'x?' convention"
    (expect (convert-identifier-conventions "foo?") equal? "foo_p")
    (expect (convert-identifier-conventions "foo?bar") equal? "foo?bar")
    )
  (it "should convert 'x!' convention"
    (expect (convert-identifier-conventions "foo!") equal? "foo_x")
    (expect (convert-identifier-conventions "foo!bar") equal? "foo!bar")
    )
  (it "should convert 'x->y' convention"
    (expect (convert-identifier-conventions "foo->bar") equal? "foo_to_bar")
    (expect (convert-identifier-conventions "x->y->z") equal? "x_to_y_to_z")
    )
  (it "should convert 'x-y-z' convention"
    (expect (convert-identifier-conventions "x-y-z") equal? "x_y_z")
    (expect (convert-identifier-conventions "strange-") equal? "strange_")
    )
  (it "should convert '%x' convention"
    (expect (convert-identifier-conventions "%internal") equal? "_internal")
    (expect (convert-identifier-conventions "w%i%r%e%d") equal? "w_i_r_e_d")
    )
  )

(describe "convert-regexp-conventions"
  (it "should convert given Scheme regexp into Vim script string"
    (expect (convert-regexp-conventions #/foo/) equal? "'foo'")
    (expect (convert-regexp-conventions #/\(x\|y\)/) equal? "'\\(x\\|y\\)'")
    (expect (convert-regexp-conventions #/'/) equal? "''''")
    )
  )

(describe "convert-string-conventions"
  (it "should convert given Scheme string into Vim script string"
    (expect (convert-string-conventions "foo") equal? "\"foo\"")
    (expect (convert-string-conventions "f\"oo") equal? "\"f\\\"oo\"")
    (expect (convert-string-conventions "f'oo") equal? "\"f'oo\"")
    (expect (convert-string-conventions "-\x1f-") equal? "\"-\\x1f-\"")
    )
  )

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

(describe "scheme-object->vim-script-notation"
  (it "should convert given Scheme boolean into equivalent one in Vim script"
    (expect (scheme-object->vim-script-notation #f) equal? 0)
    (expect (scheme-object->vim-script-notation #t) equal? 1)
    )
  (it "should convert given Scheme number into equivalent one in Vim script"
    (expect (scheme-object->vim-script-notation 0) equal? 0)
    (expect (scheme-object->vim-script-notation 123) equal? 123)
    (expect (scheme-object->vim-script-notation -123) equal? -123)
    (expect (scheme-object->vim-script-notation (- (ash 1 31) 1)) equal? (- (ash 1 31) 1))
    (expect (scheme-object->vim-script-notation (ash -1 31)) equal? (ash -1 31))
    (expect (scheme-object->vim-script-notation (ash 1 31)) raise?)
    (expect (scheme-object->vim-script-notation (ash -1 32)) raise?)
    (expect (scheme-object->vim-script-notation 0.123) raise?)
    (expect (scheme-object->vim-script-notation #i123) raise?)
    (expect (scheme-object->vim-script-notation 1+2i) raise?)
    )
  (it "should convert given Scheme regexp into equivalent one in Vim script"
    (expect (scheme-object->vim-script-notation #/\(foo\|bar\)/) equal? "'\\(foo\\|bar\\)'")
    )
  (it "should convert given Scheme string into equivalent one in Vim script"
    (expect (scheme-object->vim-script-notation "f\"oo") equal? "\"f\\\"oo\"")
    )
  (it "should convert given Scheme symbol into equivalent one in Vim script"
    (expect (scheme-object->vim-script-notation 'foo!) equal? "foo_x")
    )
  (it "should fail for other Scheme objects"
    (expect (scheme-object->vim-script-notation '()) raise?)
    (expect (scheme-object->vim-script-notation '(x y z)) raise?)
    (expect (scheme-object->vim-script-notation (lambda () '())) raise?)
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
