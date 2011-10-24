(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use srfi-1)
(use util.list)
(use vim.xire.compiler)
(use vim.xire.ivs)




;;; Expressions
;;; ===========

;;; Helpers
;;; -------

(define-macro (define-binary-operator name op :optional default-val)
  `(define-xire-expr ,name
     [(_ $val1:form)
      (when (undefined? ,default-val)
        (errorf "Operator ~s takes two or more arguments" ',name))
      `(,',name ,',default-val ,$val1)]
     [(_ $val1:expr $val2:expr)
      (IVS (E (Q "(")
              $val1
              (Q ,op)
              $val2
              (Q ")")))]
     [(_ $val1:form $val2:form $valN:form ...)
      `(,',name (,',name ,$val1 ,$val2)
                ,@$valN)]
     ))

(define-macro (define-comparison-operator name op)
  `(define-xire-expr ,name
     [(_ $val1:expr $val2:expr)
      (IVS (E (Q "(")
              $val1
              (Q ,op)
              $val2
              (Q ")")))]
     [(_ $val1:form $val2:form $valN:form ...)
      `(and (,',name ,$val1 ,$val2)
            (,',name ,$val2 ,@$valN))]
     ))

;;; expr1
;;; -----

(define-xire-expr if
  [(_ $cond:expr $then:expr $else:expr)
   (IVS (E (Q "(")
           $cond
           (Q "?")
           $then
           (Q " ")  ; To parse r?s:t as (r)?(s):(t) not (r)?(s:t).
           (Q ":")
           $else
           (Q ")")))]
  )

;;; expr2
;;; -----

(define-binary-operator or "||")

;;; expr3
;;; -----

(define-binary-operator and "&&")

;;; expr4
;;; -----

(define-comparison-operator != "!=")
(define-comparison-operator !=# "!=#")
(define-comparison-operator !=? "!=?")
(define-comparison-operator !~ "!~")
(define-comparison-operator !~# "!~#")
(define-comparison-operator !~? "!~?")
(define-comparison-operator < "<")
(define-comparison-operator <# "<#")
(define-comparison-operator <= "<=")
(define-comparison-operator <=# "<=#")
(define-comparison-operator <=? "<=?")
(define-comparison-operator <? "<?")
(define-comparison-operator == "==")
(define-comparison-operator ==# "==#")
(define-comparison-operator ==? "==?")
(define-comparison-operator =~ "=~")
(define-comparison-operator =~# "=~#")
(define-comparison-operator =~? "=~?")
(define-comparison-operator > ">")
(define-comparison-operator ># ">#")
(define-comparison-operator >= ">=")
(define-comparison-operator >=# ">=#")
(define-comparison-operator >=? ">=?")
(define-comparison-operator >? ">?")
(define-comparison-operator is " is ")
(define-comparison-operator is# " is# ")
(define-comparison-operator is? " is? ")
(define-comparison-operator isnot " isnot ")
(define-comparison-operator isnot# " isnot# ")
(define-comparison-operator isnot? " isnot? ")

;;; expr5
;;; -----

(define-binary-operator + "+" 0)
(define-binary-operator - "-" 0)
(define-binary-operator .. ".")

;;; expr6
;;; -----

(define-binary-operator * "*")
(define-binary-operator / "/")
(define-binary-operator % "%")

;;; expr7
;;; -----

(define-xire-expr not
  [(_ $value:expr)
   (IVS (E (Q "(")
           (Q "!")
           $value
           (Q ")")))]
  )
; Macro "-" supports both unary and binary usage.
; Macro "+" supports both unary and binary usage.

;;; expr8
;;; -----

(define-xire-expr ref
  [(_ $container:expr $index:expr)
   (IVS (E (Q "(")
           $container
           (Q "[")
           $index
           (Q "]")
           (Q ")")))]
  )

(define-xire-expr slice
  [(_ $container:expr $index-from:expr $index-to:expr)
   (IVS (E (Q "(")
           $container
           (Q "[")
           $index-from
           (Q " ")  ; To parse l[s:x] as l[(s):x] not l[(s:x)].
           (Q ":")
           $index-to
           (Q "]")
           (Q ")")))]
  )

;;; expr9
;;; -----

(define-xire-expr list
  [(_ $value:expr ...)
   (IVS (E (Q "[")
           (apply E (intersperse (Q ",") $value))
           (Q "]")))]
  )

(define-xire-expr dict
  [(_ ($key:expr $value:expr) ...)
   (IVS (E (Q "{")
           (apply IVS
                  (map (cut E
                            <>
                            (Q " ")  ; To parse {s:x} as {(s):x} not {(s:x)}.
                            (Q ":")
                            <>
                            (Q ","))
                       $key
                       $value))
           (Q "}")))]
  )




;;; Fundamental statements
;;; ======================

(define-xire-stmt begin
  [(_ $body:stmt ...)
   (apply IVS $body)]
  )

(define-xire-stmt cond
  [(_ [$cond:expr $then:stmt] ...)
   (let go ([cond:exprs $cond:expr]
            [conds $cond]
            [thens $then]
            [result '()])
     (cond
       [(null? cond:exprs)
        (if (null? result)
          (IVS)
          (apply IVS (reverse (cons (S 'endif) result))))]
       [else
         (go (cdr cond:exprs)
             (cdr conds)
             (cdr thens)
             (cons (car thens)
                   (cons (S (if (null? result)
                              'if
                              'elseif)
                            (if (and (null? (cdr cond:exprs))
                                  (eq? (car cond:exprs) 'else))
                              (E #t)
                              (car conds)))
                         result)))]))]
  )

(define-xire-stmt echo
  [(_ $value:expr ...)
   (IVS (apply S 'echo $value))]
  )

(define-xire-stmt for
  [(_ $var:expr $list:expr $body:stmt)
   (IVS (S 'for $var 'in $list)
        $body
        (S 'endfor))]
  [(_ $var:form $list:form $body:form ...)
   `(for ,$var ,$list (begin ,@$body))]
  )

(define-xire-stmt if
  [(_ $cond:expr $then:stmt)
   (IVS (S 'if $cond)
        $then
        (S 'endif))]
  [(_ $cond:expr $then:stmt $else:stmt)
   (IVS (S 'if $cond)
        $then
        (S 'else)
        $else
        (S 'endif))]
  )

(define-xire-stmt set!
  [(_ $var:expr $value:expr)
   (IVS (S 'let $var (Q '=) $value))]
  )

(define-xire-stmt when
  [(_ $cond:form $then:form ...)
   `(if ,$cond
      (begin
        ,@$then))]
  )




;;; __END__
