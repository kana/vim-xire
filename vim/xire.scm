(define-module vim.xire
  (export
    ; Public API
    =ex=

    ; Not public, but exported to test.
    ))
(select-module vim.xire)

(use srfi-1)
(use util.list)




;;; Helper API
;;; ==========

(define (=ex= . ex-cmd-ivss)
  (define (insert-newline xs)
    (map (lambda (x)
           `(,x "\n"))
         xs))
  (define (insert-space xs)
    (map (lambda (x)
           (if (list? x)
             (intersperse " " x)
             x))
         xs))
  `(()
    ,@(concatenate
        (insert-newline
          (insert-space
            ex-cmd-ivss))))
  )




(provide "vim/xire")
