(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use srfi-1)
(use vim.xire.compiler)
(use vim.xire.ivs)




;;; Expressions
;;; ===========

;;; Helpers
;;; -------

(define-macro (define-binary-operator name op)
  `(define-xire-expr ,name
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

;;; expr3
;;; -----

(define-binary-operator and "&&")




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
