#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.iform)




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




(describe "pass-final"
  (define (gen iform)
    (call-with-output-string
      (lambda (port)
        (write-tree (pass-final (list iform)) port))))
  (it "should reject an object which is not a valid iform"
    (expect (gen '#($FOO)) raise? <error>)
    (expect (gen '#($CONST 1 2)) raise? <error>)
    )
  (it "should generate a valid code from $CONST"
    (expect (gen (make-const #f)) equal? "0")
    (expect (gen (make-const #t)) equal? "1")
    (expect (gen (make-const 123)) equal? "123")
    (expect (gen (make-const "abc")) equal? "\"abc\"")
    (expect (gen (make-const #/regexp/)) equal? "'regexp'")
    )
  (it "should generate a valid code from $GREF"
    (expect (gen (make-gref 'g:var)) equal? "g:var")
    (expect (gen (make-gref 'g:foo-bar)) equal? "g:foo_bar")
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
