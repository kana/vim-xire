#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.util)




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

(describe "convert-key-sequence-conventions"
  (it "should translate key sequence string properly"
    (expect (convert-key-sequence-conventions "foo")
            equal? "\"foo\"")
    (expect (convert-key-sequence-conventions "\"*p")
            equal? "\"\\\"*p\"")
    (expect (convert-key-sequence-conventions "bar<BS>z")
            equal? "\"bar\\<BS>z\"")
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




(run-suites)

; __END__
; vim: filetype=scheme
