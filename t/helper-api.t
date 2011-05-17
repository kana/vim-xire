#!/usr/bin/env gosh

(add-load-path ".")
(add-load-path "./gauche-test-gasmine")

(use test.gasmine)
(use vim.xire)




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




(run-suites)

; vim: filetype=scheme
