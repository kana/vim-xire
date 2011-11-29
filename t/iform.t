#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.iform)
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




(describe "iform?"
  (it "should distinguish a valid iform"
    (expect (iform? (make-const 0)) eq? #t)
    (expect (iform? '($CONST 0)) eq? #f)
    (expect (iform? '#()) eq? #t)  ; Though it should be #f.
    )
  )

(describe "iform-tag"
  (it "should extract a tag from a given iform"
    (expect (iform-tag (make-const 0)) eq? '$CONST)
    (expect (iform-tag '($CONST 0)) raise? <error>)
    (expect (iform-tag '#()) raise? <error>)
    )
  )




(describe "make-def"
  (it "should make a iform for $DEF"
    (expect (make-def 'g:var (make-const 0))
            equal?
            '#($DEF g:var #($CONST 0)))
    )
  )




(describe "make-gset"
  (it "should make a iform for $GSET"
    (expect (make-gset 'g:var (make-const 0))
            equal?
            '#($GSET g:var #($CONST 0)))
    )
  )

(describe "make-let"
  (it "should make a iform for $LET"
    (expect (make-let '(var1 var2)
                      (list (make-const 1) (make-const 2))
                      (make-gset 'g:var (make-const 0)))
            equal?
            '#($LET (var1 var2)
                    (#($CONST 1) #($CONST 2))
                    #($GSET g:var #($CONST 0))))
    )
  )

(describe "make-lset"
  (it "should make a iform for $LSET"
    (expect (make-lset 'var (make-const 0))
            equal?
            '#($LSET var #($CONST 0)))
    )
  )

(describe "make-begin"
  (it "should make a iform for $BEGIN"
    (expect (make-begin (list (make-gset 'g:var1 (make-const 1))
                              (make-gset 'g:var2 (make-const 2))))
            equal?
            '#($BEGIN (#($GSET g:var1 #($CONST 1))
                       #($GSET g:var2 #($CONST 2)))))
    )
  )

(describe "make-if"
  (it "should make a iform for $IF"
    (expect (make-if (make-const 0)
                     (make-gset 'g:var1 (make-const 1))
                     (make-gset 'g:var2 (make-const 2)))
            equal?
            '#($IF #($CONST 0)
                   #($GSET g:var1 #($CONST 1))
                   #($GSET g:var2 #($CONST 2))))
    )
  )

(describe "make-while"
  (it "should make a iform for $WHILE"
    (expect (make-while (make-const 0)
                        (make-gset 'g:var1 (make-const 1)))
            equal?
            '#($WHILE #($CONST 0)
                      #($GSET g:var1 #($CONST 1))))
    )
  )

(describe "make-for"
  (it "should make a iform for $FOR"
    (expect (make-for 'var
                      (make-const '(0 1 2))
                      (make-gset 'g:var1 (make-const 1)))
            equal?
            '#($FOR var
                    #($CONST (0 1 2))
                    #($GSET g:var1 #($CONST 1))))
    )
  )

(describe "make-break"
  (it "should make a iform for $BREAK"
    (expect (make-break)
            equal?
            '#($BREAK))
    )
  )

(describe "make-next"
  (it "should make a iform for $NEXT"
    (expect (make-next)
            equal?
            '#($NEXT))
    )
  )

(describe "make-ret"
  (it "should make a iform for $RET"
    (expect (make-ret (make-const 0))
            equal?
            '#($RET #($CONST 0)))
    )
  )

(describe "make-func"
  (it "should make a iform for $FUNC"
    (expect (make-func 'f '(a b c) (make-gset 'g:var (make-const 0)))
            equal?
            '#($FUNC f (a b c) #($GSET g:var #($CONST 0))))
    )
  )

(describe "make-ex"
  (it "should make a iform for $EX"
    (expect (make-ex (list 'foo (make-const 0)))
            equal?
            '#($EX (foo #($CONST 0))))
    )
  )




(describe "make-const"
  (it "should make a iform for $CONST"
    (expect (make-const 0)
            equal?
            '#($CONST 0))
    )
  )

(describe "make-gref"
  (it "should make a iform for $GREF"
    (expect (make-gref 'g:var)
            equal?
            '#($GREF g:var))
    )
  )

(describe "make-lref"
  (it "should make a iform for $LREF"
    (expect (make-lref 'var)
            equal?
            '#($LREF var))
    )
  )

(describe "make-call"
  (it "should make a iform for $CALL"
    (expect (make-call (make-gref 'bufnr) (list (make-const "$")))
            equal?
            '#($CALL #($GREF bufnr) (#($CONST "$"))))
    (expect (make-call '+ (list (make-const 1) (make-const 2)))
            equal?
            '#($CALL + (#($CONST 1) #($CONST 2))))
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
