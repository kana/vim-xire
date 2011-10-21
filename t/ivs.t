#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.ivs)




(define (translate x)
  (call-with-output-string
    (cut write-tree x <>)))




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




(describe "Q"
  (it "should be translated its content as is"
    (expect (translate (Q 123 "abc" 'foo->bar #/\<regexp\>/))
            equal?
            "123abcfoo->bar#/\\<regexp\\>/")
    )
  (it "should not be translated properly if it has any non-atom"
    (define x (Q 'x))
    (expect (translate (Q x))
            equal?
            (write-to-string x display))
    )
  (it "should be equal? to other node if both contents are equal?"
    (expect (Q (list 1 2 3))
            equal?
            (Q (list 1 2 3)))
    )
  )

(describe "E"
  (it "should be translated without a trailing newline"
    (expect (translate (E 'scriptnames)) equal? "scriptnames")
    )
  (it "should be translated its content without spaces"
    (expect (translate (E 123 456 789)) equal? "123456789")
    )
  (it "should be translated its content in Vim script notation"
    (expect (translate (E 123)) equal? "123")
    (expect (translate (E "abc")) equal? "\"abc\"")
    (expect (translate (E 'foo->bar)) equal? "foo_to_bar")
    (expect (translate (E #/\<regexp\>/)) equal? "'\\<regexp\\>'")
    )
  (it "should be translated properly if its content is also a node"
    (expect (translate (E 'a? (E 'b?) 'c?)) equal? "a_pb_pc_p")
    )
  (it "should be equal? to other node if both contents are equal?"
    (expect (E (list 1 2 3))
            equal?
            (E (list 1 2 3)))
    )
  )

(describe "S"
  (it "should be translated with a trailing newline"
    (expect (translate (S 'scriptnames)) equal? "scriptnames\n")
    )
  (it "should be translated its content with spaces"
    (expect (translate (S 123 456 789)) equal? "123 456 789\n")
    )
  (it "should be translated its content in Vim script notation"
    (expect (translate (S 123)) equal? "123\n")
    (expect (translate (S "abc")) equal? "\"abc\"\n")
    (expect (translate (S 'foo->bar)) equal? "foo_to_bar\n")
    (expect (translate (S #/\<regexp\>/)) equal? "'\\<regexp\\>'\n")
    )
  (it "should be translated properly if its content is also a node"
    (expect (translate (S 'a? (E 'b?) 'c?)) equal? "a_p b_p c_p\n")
    )
  (it "should be equal? to other node if both contents are equal?"
    (expect (S (list 1 2 3))
            equal?
            (S (list 1 2 3)))
    )
  )

(describe "IVS"
  (it "should be translated without any addition"
      (expect (translate (IVS (S 'if (E #t))
                              (S 'echo (E 'then))
                              (S 'endif)))
              equal?
            (string-append
              (translate (S 'if (E #t)))
              (translate (S 'echo (E 'then)))
              (translate (S 'endif))))
    )
  (it "should be equal? to other node if both contents are equal?"
    (expect (IVS (Q (list 1 2 3)))
            equal?
            (IVS (Q (list 1 2 3))))
    )
  (it "should accept only IVS objects"
    (expect (IVS (Q)) not raise?)
    (expect (IVS (E)) not raise?)
    (expect (IVS (S)) not raise?)
    (expect (IVS (IVS (S))) not raise?)
    (expect (IVS "non-IVS objects") raise?)
    )
  )




(run-suites)

; vim: filetype=scheme
