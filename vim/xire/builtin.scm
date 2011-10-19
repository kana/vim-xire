(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use srfi-1)
(use vim.xire.compiler)




(define-xire-stmt begin
  [(begin $body:stmt ...)
   (apply =ex= $body)]
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
          (=ex=)
          (apply =ex= (reverse (cons 'endif result))))]
       [else
         (go (cdr cond:exprs)
             (cdr conds)
             (cdr thens)
             (cons (car thens)
                   (cons (list (if (null? result)
                                 'if
                                 'elseif)
                               (if (and (null? (cdr cond:exprs))
                                        (eq? (car cond:exprs) 'else))
                                 1
                                 (car conds)))
                         result)))]))]
  )

(define-xire-stmt echo
  [(echo $value:expr ...)
   (=ex= `(echo ,@$value))]
  )

(define-xire-stmt if
  [(if $cond:expr $then:stmt)
   (=ex= `(if ,$cond)
         $then
         'endif)]
  [(if $cond:expr $then:stmt $else:stmt)
   (=ex= `(if ,$cond)
         $then
         'else
         $else
         'endif)]
  )

(define-xire-stmt set!
  [(set! $var:expr $value:expr)
   (=ex= `(let ,$var = ,$value))]
  )

(define-xire-stmt when
  [(when $cond:expr $then:stmt ...)
   `(if ,$cond
      (begin
        ,@$then))]
  )




;;; __END__
