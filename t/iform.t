#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.iform)
(use vim.xire.util)




(define (make-lvar src-name :optional (new-name (gensym)) init-expr)
  (make <lvar>
        :src-name src-name
        :new-name new-name
        :init-expr init-expr))




(describe "iform?"
  (it "should distinguish a valid iform"
    (expect (iform? ($const 0)) eq? #t)
    (expect (iform? '($CONST 0)) eq? #f)
    (expect (iform? '#()) eq? #t)  ; Though it should be #f.
    )
  )

(describe "iform-tag"
  (it "should extract a tag from a given iform"
    (expect (iform-tag ($const 0)) eq? '$CONST)
    (expect (iform-tag '($CONST 0)) raise? <error>)
    (expect (iform-tag '#()) raise? <error>)
    )
  )




(describe "$def"
  (it "should make a iform for $DEF"
    (expect ($def 'g:var ($const 0))
            equal?
            '#($DEF g:var #($CONST 0)))
    )
  )




(describe "$gset"
  (it "should make a iform for $GSET"
    (expect ($gset 'g:var ($const 0))
            equal?
            '#($GSET g:var #($CONST 0)))
    )
  )

(describe "$let"
  (it "should make a iform for $LET"
    (define lvar1 (make-lvar 'foo (gensym) ($const 1)))
    (define lvar2 (make-lvar 'bar (gensym) ($const 2)))
    (expect ($let (list lvar1 lvar2)
                  ($gset 'g:var ($const 0)))
            equal?
            `#($LET (,lvar1 ,lvar2)
                    #($GSET g:var #($CONST 0))))
    )
  )

(describe "$lset"
  (it "should make a iform for $LSET"
    (define lvar (make-lvar 'foo (gensym) ($const 1)))
    (expect ($lset lvar ($const 0))
            equal?
            `#($LSET ,lvar #($CONST 0)))
    )
  )

(describe "$begin"
  (it "should make a iform for $BEGIN"
    (expect ($begin (list ($gset 'g:var1 ($const 1))
                          ($gset 'g:var2 ($const 2))))
            equal?
            '#($BEGIN (#($GSET g:var1 #($CONST 1))
                       #($GSET g:var2 #($CONST 2)))))
    )
  )

(describe "$if"
  (it "should make a iform for $IF"
    (expect ($if ($const 0)
                 ($gset 'g:var1 ($const 1))
                 ($gset 'g:var2 ($const 2)))
            equal?
            '#($IF #($CONST 0)
                   #($GSET g:var1 #($CONST 1))
                   #($GSET g:var2 #($CONST 2))))
    )
  )

(describe "$while"
  (it "should make a iform for $WHILE"
    (expect ($while ($const 0)
                    ($gset 'g:var1 ($const 1)))
            equal?
            '#($WHILE #($CONST 0)
                      #($GSET g:var1 #($CONST 1))))
    )
  )

(describe "$for"
  (it "should make a iform for $FOR"
    (define lvar (make-lvar 'foo (gensym) ($const 1)))
    (expect ($for lvar
                  ($const 0)
                  ($gset 'g:var1 ($const 1)))
            equal?
            `#($FOR ,lvar
                    #($CONST 0)
                    #($GSET g:var1 #($CONST 1))))
    )
  )

(describe "$break"
  (it "should make a iform for $BREAK"
    (expect ($break)
            equal?
            '#($BREAK))
    )
  )

(describe "$next"
  (it "should make a iform for $NEXT"
    (expect ($next)
            equal?
            '#($NEXT))
    )
  )

(describe "$ret"
  (it "should make a iform for $RET"
    (expect ($ret ($const 0))
            equal?
            '#($RET #($CONST 0)))
    )
  )

(describe "$func"
  (it "should make a iform for $FUNC"
    (define a (make <lvar> :src-name 'a :new-name 'a:A :src-name 'A))
    (define b (make <lvar> :src-name 'b :new-name 'a:B :src-name 'B))
    (define c (make <lvar> :src-name 'c :new-name 'a:C :src-name 'C))
    (expect ($func 'f (list a b c) ($gset 'g:var ($const 0)))
            equal?
            `#($FUNC f (,a ,b ,c) #($GSET g:var #($CONST 0))))
    )
  )

(describe "$ex"
  (it "should make a iform for $EX"
    (expect ($ex (list 'foo ($const 0)))
            equal?
            '#($EX (foo #($CONST 0))))
    )
  )




(describe "$const"
  (it "should make a iform for $CONST"
    (expect ($const 0)
            equal?
            '#($CONST 0))
    )
  )

(describe "$gref"
  (it "should make a iform for $GREF"
    (expect ($gref 'g:var)
            equal?
            '#($GREF g:var))
    )
  )

(describe "$lref"
  (it "should make a iform for $LREF"
    (define lvar (make-lvar 'foo (gensym)))
    (expect ($lref lvar)
            equal?
            `#($LREF ,lvar))
    )
  )

(describe "$call"
  (it "should make a iform for $CALL"
    (expect ($call ($gref 'bufnr) (list ($const "$")))
            equal?
            '#($CALL #($GREF bufnr) (#($CONST "$"))))
    (expect ($call '+ (list ($const 1) ($const 2)))
            equal?
            '#($CALL + (#($CONST 1) #($CONST 2))))
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
