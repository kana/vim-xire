#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

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
  )

(describe "scheme->ivs"
  (it "should convert given Scheme boolean into equivalent one in Vim script"
    (expect (scheme->ivs #f) equal? 0)
    (expect (scheme->ivs #t) equal? 1)
    )
  (it "should convert given Scheme number into equivalent one in Vim script"
    (expect (scheme->ivs 0) equal? 0)
    (expect (scheme->ivs 123) equal? 123)
    (expect (scheme->ivs -123) equal? -123)
    (expect (scheme->ivs (- (ash 1 31) 1)) equal? (- (ash 1 31) 1))
    (expect (scheme->ivs (ash -1 31)) equal? (ash -1 31))
    (expect (scheme->ivs (ash 1 31)) raise?)
    (expect (scheme->ivs (ash -1 32)) raise?)
    (expect (scheme->ivs 0.123) raise?)
    (expect (scheme->ivs #i123) raise?)
    (expect (scheme->ivs 1+2i) raise?)
    )
  (it "should convert given Scheme regexp into equivalent one in Vim script"
    (expect (scheme->ivs #/\(foo\|bar\)/) equal? "'\\(foo\\|bar\\)'")
    )
  (it "should convert given Scheme string into equivalent one in Vim script"
    (expect (scheme->ivs "f\"oo") equal? "\"f\\\"oo\"")
    )
  (it "should convert given Scheme symbol into equivalent one in Vim script"
    (expect (scheme->ivs 'foo!) equal? "foo_x")
    )
  (it "should fail for other Scheme objects"
    (expect (scheme->ivs '()) raise?)
    (expect (scheme->ivs '(x y z)) raise?)
    (expect (scheme->ivs (lambda () '())) raise?)
    )
  )




(run-suites)

; vim: filetype=scheme
