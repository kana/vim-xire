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




;;; __END__
