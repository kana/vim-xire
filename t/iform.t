#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.iform)




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




(describe "pass-final"
  (define (gen iform . args)
    (call-with-output-string
      (lambda (port)
        (write-tree (apply pass-final (list iform) args) port))))
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
  (it "should generate a valid code from $LREF"
    (define state (make <pass-final/state>
                        :lvars '((var . var123)
                                 (foo-bar . foobar123))))
    (expect (gen (make-lref 'var) state) equal? "var123")
    (expect (gen (make-lref 'foo-bar) state) equal? "foobar123")
    (expect (gen (make-lref 'undefined) state) raise? <error>)
    )
  (it "should generate a valid code from $CALL of a function"
    (expect (gen (make-call (make-gref 'changenr)
                            (list)))
            equal? "changenr()")
    (expect (gen (make-call (make-gref 'bufnr)
                            (list (make-const "$"))))
            equal? "bufnr(\"$\")")
    (expect (gen (make-call (make-gref 'fnamemodify)
                            (list (make-const "main.c")
                                  (make-const ":p:h"))))
            equal? "fnamemodify(\"main.c\",\":p:h\")")
    )
  (it "should not generate a code from $CALL of an invalid operator"
    (expect (gen (make-call 'an-invalid-operator
                            (list (make-const 1)
                                  (make-const 2))))
            raise? <error>)
    )
  (it "should generate a valid code from $CALL of the built-in 'if'"
    (expect (gen (make-call 'if
                            (list (make-const 1)
                                  (make-const 2)
                                  (make-const 3))))
            equal? "(1 ? 2 : 3)")
    (expect (gen (make-call 'if
                            (list (make-const 1)
                                  (make-const 2))))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-call 'if
                            (list (make-const 1)
                                  (make-const 2)
                                  (make-const 3)
                                  (make-const 4))))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-call 'if
                            (list 1
                                  (make-const 2)
                                  (make-const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'if
                            (list (make-const 1)
                                  2
                                  (make-const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'if
                            (list (make-const 1)
                                  (make-const 2)
                                  3)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of most binary operators"
    (for-each
      (lambda (op)
        (expect (gen (make-call (car op)
                                (list (make-const 1)
                                      (make-const 2))))
                equal? #`"(1 ,(cdr op) 2)")
        (expect (gen (make-call (car op)
                                (list (make-const 1))))
                raise? <error>)  ; Too few arguments.
        (expect (gen (make-call (car op)
                                (list (make-const 1)
                                      (make-const 2)
                                      (make-const 3))))
                raise? <error>)  ; Too many arguments.
        (expect (gen (make-call (car op)
                                (list 1
                                      (make-const 2))))
                raise? <error>)  ; Non-iform arguments.
        (expect (gen (make-call (car op)
                                (list (make-const 1)
                                      2)))
                raise? <error>)  ; Non-iform arguments.
        )
      bin-op-table)
    )
  (it "should generate a valid code from $CALL of built-in unary operators"
    (for-each
      (lambda (op)
        (expect (gen (make-call (car op)
                                (list (make-const 1))))
                equal? #`"(,(cdr op)1)")
        (expect (gen (make-call (car op)
                                (list)))
                raise? <error>)  ; Too few arguments.
        (expect (gen (make-call (car op)
                                (list (make-const 1)
                                      (make-const 2))))
                raise? <error>)  ; Too many arguments.
        (expect (gen (make-call (car op)
                                (list 1)))
                raise? <error>)  ; Non-iform arguments.
        )
      un-op-table)
    )
  (it "should generate a valid code from $CALL of the built-in 'ref'"
    (expect (gen (make-call 'ref
                            (list (make-const 1)
                                  (make-const 2))))
            equal? "(1[2])")
    (expect (gen (make-call 'ref
                            (list (make-const 1))))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-call 'ref
                            (list (make-const 1)
                                  (make-const 2)
                                  (make-const 3))))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-call 'ref
                            (list 1
                                  (make-const 2))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'ref
                            (list (make-const 1)
                                  2)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in 'slice'"
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  (make-const 2)
                                  (make-const 3))))
            equal? "(1[2 : 3])")
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  #f
                                  (make-const 3))))
            equal? "(1[ : 3])")
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  (make-const 2)
                                  #f)))
            equal? "(1[2 : ])")
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  (make-const 2))))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  (make-const 2)
                                  (make-const 3)
                                  (make-const 4))))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-call 'slice
                            (list 1
                                  (make-const 2)
                                  (make-const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  2
                                  (make-const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'slice
                            (list (make-const 1)
                                  (make-const 2)
                                  3)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in '->'"
    (expect (gen (make-call '->
                            (list (make-const 1)
                                  'name)))
            equal? "(1.name)")
    (expect (gen (make-call '->
                            (list (make-const 1))))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-call '->
                            (list (make-const 1)
                                  'name
                                  (make-const 3))))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-call '->
                            (list 1
                                  'name)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in 'list'"
    (expect (gen (make-call 'list
                            (list)))
            equal? "[]")
    (expect (gen (make-call 'list
                            (list (make-const 1))))
            equal? "[1]")
    (expect (gen (make-call 'list
                            (list (make-const 1)
                                  (make-const 2))))
            equal? "[1,2]")
    (expect (gen (make-call 'list
                            (list 1)))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'list
                            1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in 'dict'"
    (expect (gen (make-call 'dict
                            (list '()
                                  '())))
            equal? "{}")
    (expect (gen (make-call 'dict
                            (list (list (make-const "1"))
                                  (list (make-const "a")))))
            equal? "{\"1\" : \"a\"}")
    (expect (gen (make-call 'dict
                            (list (list (make-const "1")
                                        (make-const "2"))
                                  (list (make-const "a")
                                        (make-const "b")))))
            equal? "{\"1\" : \"a\",\"2\" : \"b\"}")
    (expect (gen (make-call 'dict
                            (list (list 1)
                                  (list (make-const "a")))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-call 'dict
                            (list (list (make-const "1"))
                                  (list "a"))))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $DEF"
    (expect (gen (make-def 'foo-bar (make-const 1)))
            equal? "let foo_bar=1\n")
    (expect (gen (make-def 'foo-bar))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-def 'foo-bar (make-const 1)
                                    (make-const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-def 'foo-bar 1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $GSET"
    (expect (gen (make-gset 'foo-bar (make-const 1)))
            equal? "let foo_bar=1\n")
    (expect (gen (make-gset 'foo-bar))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-gset 'foo-bar (make-const 1)
                                     (make-const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-gset 'foo-bar 1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $LSET"
    (define state (make <pass-final/state>
                        :lvars '((foo-bar . foobar123))))
    (expect (gen (make-lset 'foo-bar (make-const 1))
                 state)
            equal? "let foobar123=1\n")
    (expect (gen (make-lset 'undefined (make-const 1))
                 state)
            raise? <error>)  ; Undefined variable.
    (expect (gen (make-lset 'foo-bar)
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-lset 'foo-bar (make-const 1)
                                     (make-const 2))
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-lset 'foo-bar 1)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $LET"
    (define state (make <pass-final/state>
                        :lvars '((x . OUTER_X))))
    (expect (gen (make-let '()
                           '()
                           (make-lset 'x (make-const 999)))
                 state)
            equal? "let OUTER_X=999\n")
    (expect (gen (make-let '(x)
                           (list (make-lref 'x))
                           (make-lset 'x (make-lref 'x)))
                 state)
            #/let (s:__L\d+)=OUTER_X\nlet \1=\1\n/)
    (expect (gen (make-let '(x y)
                           (list (make-const 1) (make-const 2))
                           (make-lset 'x (make-const 999)))
                 state)
            #/let (s:__L\d+)=1\nlet (s:__L\d+)=2\nlet \1=999\n/)
    (expect (gen (make-let '(x)
                           (list (make-lref 'x))
                           (make-lset 'x (make-lref 'x)))
                 (derive-state state 'in-funcp #t))
            #/let (L\d+)=OUTER_X\nlet \1=\1\n/)
    (expect (gen (make-let '(x)
                           (list (make-lref 'x))
                           (make-lset 'x (make-lref 'x)))
                 (derive-state state 'in-scriptp #f))
            raise? <error>)  ; Invalid context to use $LET.
    (expect (gen (make-let '()
                           '())
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-let '()
                           '()
                           (make-lset 'foo-bar (make-const 999))
                           '())
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-let 0
                           '()
                           (make-lset 'foo-bar (make-const 999)))
                 state)
            raise? <error>)  ; Invalid arguments.
    (expect (gen (make-let '()
                           0
                           (make-lset 'foo-bar (make-const 999)))
                 state)
            raise? <error>)  ; Invalid arguments.
    (expect (gen (make-let '()
                           '()
                           0)
                 state)
            raise? <error>)  ; Invalid arguments.
    )
  (it "should generate a valid code from $BEGIN"
    (define state (make <pass-final/state>
                        :lvars '((foo . FOO)
                                 (bar . BAR))))
    (expect (gen (make-begin '())
                 state)
            equal? "")
    (expect (gen (make-begin (list (make-lset 'foo (make-const 1))))
                 state)
            equal? "let FOO=1\n")
    (expect (gen (make-begin (list (make-lset 'foo (make-const 1))
                                   (make-lset 'bar (make-const 2))))
                 state)
            equal? "let FOO=1\nlet BAR=2\n")
    (expect (gen (make-begin)
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-begin '() '())
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-begin (list 0))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-begin 0)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $IF"
    (define state (make <pass-final/state>
                        :lvars '((t . T)
                                 (e . E))))
    (expect (gen (make-if (make-const 0)
                          (make-lset 't (make-const 1))
                          (make-lset 'e (make-const 2)))
                 state)
            equal? "if 0\nlet T=1\nelse\nlet E=2\nendif\n")
    (expect (gen (make-if (make-const 0)
                          (make-lset 't (make-const 1)))
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-if (make-const 0)
                          (make-lset 't (make-const 1))
                          (make-lset 'e (make-const 2))
                          0)
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-if 0
                          (make-lset 't (make-const 1))
                          (make-lset 'e (make-const 2)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-if (make-const 0)
                          1
                          (make-lset 'e (make-const 2)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-if (make-const 0)
                          (make-lset 't (make-const 1))
                          2)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $WHILE"
    (define state (make <pass-final/state>
                        :lvars '((t . T))))
    (expect (gen (make-while (make-const 0)
                             (make-lset 't (make-const 1)))
                 state)
            equal? "while 0\nlet T=1\nendwhile\n")
    (expect (gen (make-while (make-const 0))
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-while (make-const 0)
                             (make-lset 't (make-const 1))
                             2)
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-while 0
                             (make-lset 't (make-const 1)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-while (make-const 0)
                             1)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $FOR"
    (define state (make <pass-final/state>
                        :lvars '((i . I))
                        :in-funcp #t))
    (expect (gen (make-for 'i
                           (make-lref 'i)
                           (make-lset 'i (make-lref 'i)))
                 state)
            #/for (L\d+) in I\nlet \1=\1\nendfor\n/)
    (expect (gen (make-for 'i
                           (make-const 0))
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-for 'i
                           (make-const 0)
                           (make-lset 'i (make-const 1))
                           0)
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-for 'i
                           0
                           (make-lset 'i (make-const 1)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-for 'i
                           (make-const 0)
                           0)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $BREAK"
    (expect (gen (make-break))
            equal? "break\n")
    (expect (gen (make-break 0))
            raise? <error>)  ; Too many arguments.
    )
  (it "should generate a valid code from $NEXT"
    (expect (gen (make-next))
            equal? "continue\n")
    (expect (gen (make-next 0))
            raise? <error>)  ; Too many arguments.
    )
  (it "should generate a valid code from $RET"
    (expect (gen (make-ret (make-const 1)))
            equal? "return 1\n")
    (expect (gen (make-ret))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-ret (make-const 1)
                           (make-const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-ret 1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $FUNC"
    (expect (gen (make-func 'foo-bar
                            '(a b c)
                            (make-ret (make-const 1))))
            equal? "function! foo_bar(a,b,c)\nreturn 1\nendfunction\n")
    (expect (gen (make-func 'foo-bar
                            '(a b c)))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-func 'foo-bar
                            '(a b c)
                            (make-ret (make-const 1))
                            0))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-func 0
                            '(a b c)
                            (make-ret (make-const 1))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-func 'foo-bar
                            0
                            (make-ret (make-const 1))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen (make-func 'foo-bar
                            '(a b c)
                            0))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $EX"
    (expect (gen (make-ex '()))
            equal? "\n")
    (expect (gen (make-ex '(foo-bar)))
            equal? "foo-bar\n")
    (expect (gen (make-ex '(foo "bar" 123)))
            equal? "foo bar 123\n")
    (expect (gen (make-ex '(foo ("bar" (123) 456))))
            equal? "foo bar123456\n")
    (expect (gen (make-ex (list (make-gref 'foo-bar))))
            equal? "foo_bar\n")
    (expect (gen (make-ex (list (list (make-gref 'foo-bar)
                                      123)
                                (make-gref 'bar-baz))))
            equal? "foo_bar123 bar_baz\n")
    (expect (gen (make-ex))
            raise? <error>)  ; Too few arguments.
    (expect (gen (make-ex '() '()))
            raise? <error>)  ; Too many arguments.
    (expect (gen (make-ex 123))
            raise? <error>)  ; Invalid arguments.
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
