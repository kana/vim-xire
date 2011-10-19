(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use vim.xire.compiler)




(define-xire-stmt echo
  [(echo $value:expr ...)
   (=ex= `(echo ,@$value))]
  )




;;; __END__
