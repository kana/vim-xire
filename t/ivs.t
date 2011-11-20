#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.ivs)




(define (translate x)
  (call-with-output-string
    (cut write-tree x <>)))




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

; __END__
; vim: filetype=scheme
