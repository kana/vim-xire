#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use text.tree)
(use vim.xire.compiler.pass-final)
(use vim.xire.iform)




(define (make-lvar src-name :optional (new-name (gensym)) init-expr)
  (make <lvar>
        :src-name src-name
        :new-name new-name
        :init-expr init-expr))




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
    (expect (gen ($const #f)) equal? "0")
    (expect (gen ($const #t)) equal? "1")
    (expect (gen ($const 123)) equal? "123")
    (expect (gen ($const "abc")) equal? "\"abc\"")
    (expect (gen ($const #/regexp/)) equal? "'regexp'")
    )
  (it "should generate a valid code from $GREF"
    (expect (gen ($gref 'g:var)) equal? "g:var")
    (expect (gen ($gref 'g:foo-bar)) equal? "g:foo_bar")
    )
  (it "should generate a valid code from $LREF"
    (define state (make <pass-final/state>
                        :lvars '((var . var123)
                                 (foo-bar . foobar123))))
    (expect (gen ($lref 'var) state) equal? "var123")
    (expect (gen ($lref 'foo-bar) state) equal? "foobar123")
    (expect (gen ($lref 'undefined) state) raise? <error>)
    )
  (it "should generate a valid code from $LREF~"
    (expect (gen ($lref~ (make-lvar 'var 'var1))) equal? "var1")
    (expect (gen ($lref~ (make-lvar 'foo-bar 'foobar1))) equal? "foobar1")
    )
  (it "should generate a valid code from $CALL of a function"
    (expect (gen ($call ($gref 'changenr)
                        (list)))
            equal? "changenr()")
    (expect (gen ($call ($gref 'bufnr)
                        (list ($const "$"))))
            equal? "bufnr(\"$\")")
    (expect (gen ($call ($gref 'fnamemodify)
                        (list ($const "main.c")
                              ($const ":p:h"))))
            equal? "fnamemodify(\"main.c\",\":p:h\")")
    )
  (it "should not generate a code from $CALL of an invalid operator"
    (expect (gen ($call 'an-invalid-operator
                        (list ($const 1)
                              ($const 2))))
            raise? <error>)
    )
  (it "should generate a valid code from $CALL of the built-in 'if'"
    (expect (gen ($call 'if
                        (list ($const 1)
                              ($const 2)
                              ($const 3))))
            equal? "(1 ? 2 : 3)")
    (expect (gen ($call 'if
                        (list ($const 1)
                              ($const 2))))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($call 'if
                        (list ($const 1)
                              ($const 2)
                              ($const 3)
                              ($const 4))))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($call 'if
                        (list 1
                              ($const 2)
                              ($const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'if
                        (list ($const 1)
                              2
                              ($const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'if
                        (list ($const 1)
                              ($const 2)
                              3)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of most binary operators"
    (for-each
      (lambda (op)
        (expect (gen ($call (car op)
                            (list ($const 1)
                                  ($const 2))))
                equal? #`"(1 ,(cdr op) 2)")
        (expect (gen ($call (car op)
                            (list ($const 1))))
                raise? <error>)  ; Too few arguments.
        (expect (gen ($call (car op)
                            (list ($const 1)
                                  ($const 2)
                                  ($const 3))))
                raise? <error>)  ; Too many arguments.
        (expect (gen ($call (car op)
                            (list 1
                                  ($const 2))))
                raise? <error>)  ; Non-iform arguments.
        (expect (gen ($call (car op)
                            (list ($const 1)
                                  2)))
                raise? <error>)  ; Non-iform arguments.
        )
      bin-op-table)
    )
  (it "should generate a valid code from $CALL of built-in unary operators"
    (for-each
      (lambda (op)
        (expect (gen ($call (car op)
                            (list ($const 1))))
                equal? #`"(,(cdr op)1)")
        (expect (gen ($call (car op)
                            (list)))
                raise? <error>)  ; Too few arguments.
        (expect (gen ($call (car op)
                            (list ($const 1)
                                  ($const 2))))
                raise? <error>)  ; Too many arguments.
        (expect (gen ($call (car op)
                            (list 1)))
                raise? <error>)  ; Non-iform arguments.
        )
      un-op-table)
    )
  (it "should generate a valid code from $CALL of the built-in 'ref'"
    (expect (gen ($call 'ref
                        (list ($const 1)
                              ($const 2))))
            equal? "(1[2])")
    (expect (gen ($call 'ref
                        (list ($const 1))))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($call 'ref
                        (list ($const 1)
                              ($const 2)
                              ($const 3))))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($call 'ref
                        (list 1
                              ($const 2))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'ref
                        (list ($const 1)
                              2)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in 'slice'"
    (expect (gen ($call 'slice
                        (list ($const 1)
                              ($const 2)
                              ($const 3))))
            equal? "(1[2 : 3])")
    (expect (gen ($call 'slice
                        (list ($const 1)
                              #f
                              ($const 3))))
            equal? "(1[ : 3])")
    (expect (gen ($call 'slice
                        (list ($const 1)
                              ($const 2)
                              #f)))
            equal? "(1[2 : ])")
    (expect (gen ($call 'slice
                        (list ($const 1)
                              ($const 2))))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($call 'slice
                        (list ($const 1)
                              ($const 2)
                              ($const 3)
                              ($const 4))))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($call 'slice
                        (list 1
                              ($const 2)
                              ($const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'slice
                        (list ($const 1)
                              2
                              ($const 3))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'slice
                        (list ($const 1)
                              ($const 2)
                              3)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in '->'"
    (expect (gen ($call '->
                        (list ($const 1)
                              'name)))
            equal? "(1.name)")
    (expect (gen ($call '->
                        (list ($const 1))))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($call '->
                        (list ($const 1)
                              'name
                              ($const 3))))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($call '->
                        (list 1
                              'name)))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in 'list'"
    (expect (gen ($call 'list
                        (list)))
            equal? "[]")
    (expect (gen ($call 'list
                        (list ($const 1))))
            equal? "[1]")
    (expect (gen ($call 'list
                        (list ($const 1)
                              ($const 2))))
            equal? "[1,2]")
    (expect (gen ($call 'list
                        (list 1)))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'list
                        1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $CALL of the built-in 'dict'"
    (expect (gen ($call 'dict
                        (list '()
                              '())))
            equal? "{}")
    (expect (gen ($call 'dict
                        (list (list ($const "1"))
                              (list ($const "a")))))
            equal? "{\"1\" : \"a\"}")
    (expect (gen ($call 'dict
                        (list (list ($const "1")
                                    ($const "2"))
                              (list ($const "a")
                                    ($const "b")))))
            equal? "{\"1\" : \"a\",\"2\" : \"b\"}")
    (expect (gen ($call 'dict
                        (list (list 1)
                              (list ($const "a")))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($call 'dict
                        (list (list ($const "1"))
                              (list "a"))))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $DEF"
    (expect (gen ($def 'foo-bar ($const 1)))
            equal? "let foo_bar=1\n")
    (expect (gen ($def 'foo-bar))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($def 'foo-bar ($const 1)
                       ($const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($def 'foo-bar 1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $GSET"
    (expect (gen ($gset 'foo-bar ($const 1)))
            equal? "let foo_bar=1\n")
    (expect (gen ($gset 'foo-bar))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($gset 'foo-bar ($const 1)
                        ($const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($gset 'foo-bar 1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $LSET"
    (define state (make <pass-final/state>
                        :lvars '((foo-bar . foobar123))))
    (expect (gen ($lset 'foo-bar ($const 1))
                 state)
            equal? "let foobar123=1\n")
    (expect (gen ($lset 'undefined ($const 1))
                 state)
            raise? <error>)  ; Undefined variable.
    (expect (gen ($lset 'foo-bar)
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen ($lset 'foo-bar ($const 1)
                        ($const 2))
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen ($lset 'foo-bar 1)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $LSET~"
    (expect (gen ($lset~ (make-lvar 'foo-bar 'foobar123)
                         ($const 1)))
            equal? "let foobar123=1\n")
    (expect (gen ($lset~ (make-lvar 'foo-bar 'foobar123)))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($lset~ (make-lvar 'foo-bar 'foobar123)
                         ($const 1)
                         ($const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($lset~ (make-lvar 'foo-bar 'foobar123)
                         1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $LET"
    (define state (make <pass-final/state>
                        :lvars '((x . OUTER_X))))
    (expect (gen ($let '()
                       '()
                       ($lset 'x ($const 999)))
                 state)
            equal? "let OUTER_X=999\n")
    (expect (gen ($let '(x)
                       (list ($lref 'x))
                       ($lset 'x ($lref 'x)))
                 state)
            #/let (s:__L\d+)=OUTER_X\nlet \1=\1\n/)
    (expect (gen ($let '(x y)
                       (list ($const 1) ($const 2))
                       ($lset 'x ($const 999)))
                 state)
            #/let (s:__L\d+)=1\nlet (s:__L\d+)=2\nlet \1=999\n/)
    (expect (gen ($let '(x)
                       (list ($lref 'x))
                       ($lset 'x ($lref 'x)))
                 (derive-state state 'in-funcp #t))
            #/let (L\d+)=OUTER_X\nlet \1=\1\n/)
    (expect (gen ($let '(x)
                       (list ($lref 'x))
                       ($lset 'x ($lref 'x)))
                 (derive-state state 'in-scriptp #f))
            raise? <error>)  ; Invalid context to use $LET.
    (expect (gen ($let '()
                       '())
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen ($let '()
                       '()
                       ($lset 'foo-bar ($const 999))
                       '())
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen ($let 0
                       '()
                       ($lset 'foo-bar ($const 999)))
                 state)
            raise? <error>)  ; Invalid arguments.
    (expect (gen ($let '()
                       0
                       ($lset 'foo-bar ($const 999)))
                 state)
            raise? <error>)  ; Invalid arguments.
    (expect (gen ($let '()
                       '()
                       0)
                 state)
            raise? <error>)  ; Invalid arguments.
    )
  (it "should generate a valid code from $LET~"
    (define inner-x (make-lvar 'x 'INNER_X ($const 123)))
    (define outer-x (make-lvar 'x 'OUTER_X ($const 456)))
    (expect (gen ($let~ ()
                        ($lset~ outer-x ($const 999))))
            equal? "let OUTER_X=999\n")
    (expect (gen ($let~ (list inner-x)
                        ($lset~ outer-x ($lref~ inner-x))))
            equal? "let INNER_X=123\nlet OUTER_X=INNER_X\n")
    (expect (gen ($let~ (list inner-x outer-x)
                        ($lset~ inner-x ($lref~ outer-x))))
            equal? "let INNER_X=123\nlet OUTER_X=456\nlet INNER_X=OUTER_X\n")
    (expect (gen ($let~ '()))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($let~ '()
                        ($lset~ inner-x ($const 999))
                        '()))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($let~ 0
                        ($lset~ inner-x ($const 999))))
            raise? <error>)  ; Invalid arguments.
    (expect (gen ($let~ '()
                        0))
            raise? <error>)  ; Invalid arguments.
    )
  (it "should generate a valid code from $BEGIN"
    (define state (make <pass-final/state>
                        :lvars '((foo . FOO)
                                 (bar . BAR))))
    (expect (gen ($begin '())
                 state)
            equal? "")
    (expect (gen ($begin (list ($lset 'foo ($const 1))))
                 state)
            equal? "let FOO=1\n")
    (expect (gen ($begin (list ($lset 'foo ($const 1))
                               ($lset 'bar ($const 2))))
                 state)
            equal? "let FOO=1\nlet BAR=2\n")
    (expect (gen ($begin)
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen ($begin '() '())
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen ($begin (list 0))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($begin 0)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $IF"
    (define state (make <pass-final/state>
                        :lvars '((t . T)
                                 (e . E))))
    (expect (gen ($if ($const 0)
                      ($lset 't ($const 1))
                      ($lset 'e ($const 2)))
                 state)
            equal? "if 0\nlet T=1\nelse\nlet E=2\nendif\n")
    (expect (gen ($if ($const 0)
                      ($lset 't ($const 1)))
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen ($if ($const 0)
                      ($lset 't ($const 1))
                      ($lset 'e ($const 2))
                      0)
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen ($if 0
                      ($lset 't ($const 1))
                      ($lset 'e ($const 2)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($if ($const 0)
                      1
                      ($lset 'e ($const 2)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($if ($const 0)
                      ($lset 't ($const 1))
                      2)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $WHILE"
    (define state (make <pass-final/state>
                        :lvars '((t . T))))
    (expect (gen ($while ($const 0)
                         ($lset 't ($const 1)))
                 state)
            equal? "while 0\nlet T=1\nendwhile\n")
    (expect (gen ($while ($const 0))
                 state)
            raise? <error>)  ; Too few arguments.
    (expect (gen ($while ($const 0)
                         ($lset 't ($const 1))
                         2)
                 state)
            raise? <error>)  ; Too many arguments.
    (expect (gen ($while 0
                         ($lset 't ($const 1)))
                 state)
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($while ($const 0)
                         1)
                 state)
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $FOR"
    (define i (make-lvar 'i 'I))
    (expect (gen ($for i
                       ($const 0)
                       ($lset~ i ($lref~ i))))
            equal? "for I in 0\nlet I=I\nendfor\n")
    (expect (gen ($for i
                       ($const 0)))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($for i
                       ($const 0)
                       ($lset~ i ($lref~ i))
                       0))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($for '___
                       ($const 0)
                       ($lset~ i ($lref~ i))))
            raise? <error>)  ; Invalid arguments.
    (expect (gen ($for i
                       '___
                       ($lset~ i ($lref~ i))))
            raise? <error>)  ; Invalid arguments.
    (expect (gen ($for i
                       ($const 0)
                       '___))
            raise? <error>)  ; Invalid arguments.
    )
  (it "should generate a valid code from $BREAK"
    (expect (gen ($break))
            equal? "break\n")
    (expect (gen ($break 0))
            raise? <error>)  ; Too many arguments.
    )
  (it "should generate a valid code from $NEXT"
    (expect (gen ($next))
            equal? "continue\n")
    (expect (gen ($next 0))
            raise? <error>)  ; Too many arguments.
    )
  (it "should generate a valid code from $RET"
    (expect (gen ($ret ($const 1)))
            equal? "return 1\n")
    (expect (gen ($ret))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($ret ($const 1)
                       ($const 2)))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($ret 1))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $FUNC"
    (define ax (make-lvar 'x 'a:x))
    (define a... (make-lvar 'x 'a:000))
    (define lx (make-lvar 'x 'LX ($lref~ ax)))
    (expect (gen ($func
                   'foo-bar
                   '(x ...)
                   ($let~ (list lx)
                          ($ret ($call 'list
                                       (list ($lref~ lx)
                                             ($lref~ a...)))))))
            equal?
            (string-join
              '("function! foo_bar(x,...)"
                "let LX=a:x"
                "return [LX,a:000]"
                "endfunction")
              "\n"
              'suffix))
    (expect (gen ($func 'foo-bar
                        '(a b c)))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($func 'foo-bar
                        '(a b c)
                        ($ret ($const 1))
                        0))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($func 0
                        '(a b c)
                        ($ret ($const 1))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($func 'foo-bar
                        0
                        ($ret ($const 1))))
            raise? <error>)  ; Non-iform arguments.
    (expect (gen ($func 'foo-bar
                        '(a b c)
                        0))
            raise? <error>)  ; Non-iform arguments.
    )
  (it "should generate a valid code from $EX"
    (expect (gen ($ex '()))
            equal? "\n")
    (expect (gen ($ex '(foo-bar)))
            equal? "foo-bar\n")
    (expect (gen ($ex '(foo "bar" 123)))
            equal? "foo bar 123\n")
    (expect (gen ($ex '(foo ("bar" (123) 456))))
            equal? "foo bar123456\n")
    (expect (gen ($ex (list ($gref 'foo-bar))))
            equal? "foo_bar\n")
    (expect (gen ($ex (list (list ($gref 'foo-bar)
                                  123)
                            ($gref 'bar-baz))))
            equal? "foo_bar123 bar_baz\n")
    (expect (gen ($ex))
            raise? <error>)  ; Too few arguments.
    (expect (gen ($ex '() '()))
            raise? <error>)  ; Too many arguments.
    (expect (gen ($ex 123))
            raise? <error>)  ; Invalid arguments.
    )
  )




(run-suites)

; __END__
; vim: filetype=scheme
