(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use vim.xire.compiler)




(define-xire-stmt begin
  [(begin $body:stmt ...)
   (apply =ex= $body)]
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
