(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use srfi-1)
(use vim.xire.compiler)
(use vim.xire.ivs)




(define-xire-stmt begin
  [(begin $body:stmt ...)
   (apply IVS $body)]
  )

(define-xire-stmt cond
  [(cond [$cond:expr $then:stmt] ...)
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
  [(echo $value:expr ...)
   (IVS (apply S 'echo $value))]
  )

(define-xire-stmt if
  [(if $cond:expr $then:stmt)
   (IVS (S 'if $cond)
        $then
        (S 'endif))]
  [(if $cond:expr $then:stmt $else:stmt)
   (IVS (S 'if $cond)
        $then
        (S 'else)
        $else
        (S 'endif))]
  )

(define-xire-stmt set!
  [(set! $var:expr $value:expr)
   (IVS (S 'let $var (Q '=) $value))]
  )

(define-xire-stmt when
  [(when $cond:expr $then:stmt ...)
   `(if ,$cond
      (begin
        ,@$then))]
  )




;;; __END__
