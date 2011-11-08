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

(define-xire-expr slice-until
  [(_ $container:expr $index-to:expr)
   (IVS (E (Q "(")
           $container
           (Q "[")
           (Q ":")
           $index-to
           (Q "]")
           (Q ")")))]
  )

(define-xire-expr slice-from
  [(_ $container:expr $index-from:expr)
   (IVS (E (Q "(")
           $container
           (Q "[")
           $index-from
           (Q " ")  ; To parse l[s:] as l[(s):] not l[(s:)].
           (Q ":")
           (Q "]")
           (Q ")")))]
  )

(define-xire-expr ->
  [(_ $dict:expr $name:sym)
   (IVS (E (Q "(")
           $dict
           (Q ".")
           $name
           (Q ")")))]
  )

; expr8(expr1, ...) is processed by Xire-script-to-IVS layer, not macros.

;;; expr9
;;; -----

; Number literal is processed by IVS-to-Vim-script layer.

; "String literal" is processed by IVS-to-Vim-script layer.

; 'String literal' is not supported.
;
; Currently there is no plan to add this notation, because:
; - It's necessary to customize reader to support this notation,
;   but it's a bit hard to implement.
; - This notation is mostly used to write regular expressions,
;   but Xire script provides its own notation for regular expressions.

; Supplimental notation for strings to describe key sequences.
(define-xire-expr kbd
  [(_ $string:form)
   (IVS (E (Q (convert-key-sequence-conventions $string))))])

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

; &option is treated the same as a variable.

; (expr1) is implicitly supported by Xire script syntax.

; Variable is processed by IVS-to-Vim-script layer, not macros.

; Var{ia}ble is not supported.
;
; Currently there is no plan to add this notation, because:
; - This feature is not frequently used.
; - It's easy to emulate the same behavior with other ways.

; $ENVIRONMENT_VARIABLE is treated the same as a variable.

; @r (register content) is treated the same as a variable.

; function(call) is processed by Xire-script-to-IVS layer, not macros.

; fun{ct}ion(call) is not supported as var{ia}ble is not supported.




;;; Fundamental statements
;;; ======================

(define-xire-stmt begin
  [(_ $body:stmt ...)
   (apply IVS $body)]
  )

(define-xire-stmt call
  [(_ $application:expr)
   (IVS (S 'call $application))]
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

(define-xire-stmt define
  ; FIXME: Add tests on failure cases.
  ; FIXME: Detect reassignment.  (run-time? or compile-time?)
  [(_ $var:expr $value:expr)
   (unless (not (func-ctx? ctx))
     (errorf "\"define\" is not available in functions: ~s" form))
   (IVS (S 'let $var (Q '=) $value))]
  )

(define-xire-stmt echo
  [(_ $value:expr ...)
   (IVS (apply S 'echo $value))]
  )

(define-xire-stmt function
  ; FIXME: Support !.
  ; FIXME: Support range, abort and dict.
  ; FIXME: Check values on $name and $arg.
  [(_ ($name:form $arg:form ...) $body:form ...)
   (IVS
     (S 'function $name (Q "(") (apply E (intersperse (Q ",") $arg)) (Q ")"))
     (apply IVS (xire-compile-forms $body (make-func-ctx ctx $arg)))
     (S 'endfunction)
     )]
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

(define-xire-stmt let
  ; FIXME: Add tests on failure cases.
  [(_ (($var:form $value:form) ...) $body:form ...)  ; FIXME: $var:sym?
   (unless (func-ctx? ctx)
     (errorf "\"let\" is available only in functions: ~s" form))
   (let ([old-ctx ctx]
         [new-ctx (make-local-ctx ctx $var)])
     `(begin
        ,@(let go ([vars $var]
                   [values $value]
                   [forms '()])
            (if (null? vars)
              (reverse forms)
              (go (cdr vars)
                  (cdr values)
                  (cons `(set! ,(transform-value (car vars) #f 'expr new-ctx)
                           ,(transform-value (car values) #f 'expr old-ctx))
                        forms))))
        ,@(transform-value $body #t 'stmt new-ctx)
        )
     )]
  )

(define-xire-stmt let*
  ; FIXME: Add tests on failure cases.
  [(_ (($var:form $value:form) ...) $body:form ...)  ; FIXME: $var:sym?
   (unless (func-ctx? ctx)
     (errorf "\"let*\" is available only in functions: ~s" form))
   (let go ([form `(begin ,@$body)]
            [vars (reverse $var)]
            [values (reverse $value)])
     (if (null? vars)
       form
       (go `(let ([,(car vars) ,(car values)])
              ,form)
           (cdr vars)
           (cdr values))))]
  )

; letrec and letrec* are not useful unless real closures are implemented.

(define-xire-stmt return
  [(_ $value:expr)
   (IVS (S 'return $value))]
  )

(define-xire-stmt set!
  [(_ $var:expr $value:expr)
   (IVS (S 'let $var (Q '=) $value))]
  )

(define-xire-stmt until
  [(_ $cond:form $body:form ...)
   `(while (not ,$cond) (begin ,@$body))]
  )

(define-xire-stmt when
  [(_ $cond:form $then:form ...)
   `(if ,$cond
      (begin
        ,@$then))]
  )

(define-xire-stmt while
  [(_ $cond:expr $body:stmt)
   (IVS (S 'while $cond)
        $body
        (S 'endwhile))]
  [(_ $cond:form $body:form ...)
   `(while ,$cond (begin ,@$body))]
  )




;;; __END__
