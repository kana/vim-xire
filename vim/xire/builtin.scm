(define-module vim.xire.builtin
  (export
    ))
(select-module vim.xire.builtin)

(use srfi-1)
(use util.list)
(use vim.xire.compiler)
(use vim.xire.iform)
(use vim.xire.ivs)
(use vim.xire.util)




;;; Expressions
;;; ===========

;;; Helpers
;;; -------

(define-macro (define-binary-operator name op :optional default-val)
  `(defexpr ,name
     [(_ $val1:qexpr)
      (when (undefined? ,default-val)
        (errorf "Operator ~s takes two or more arguments" ',name))
      `(,',name ,',default-val ,$val1)]
     [(_ $val1:expr $val2:expr)
      (IVS (E (Q "(")
              $val1
              (Q ,op)
              $val2
              (Q ")")))]
     [(_ $val1:qexpr $val2:qexpr $valN:qexpr ...)
      `(,',name (,',name ,$val1 ,$val2)
                ,@$valN)]
     ))

(define-macro (define-comparison-operator name op)
  `(defexpr ,name
     [(_ $val1:expr $val2:expr)
      (IVS (E (Q "(")
              $val1
              (Q ,op)
              $val2
              (Q ")")))]
     [(_ $val1:qexpr $val2:qexpr $valN:qexpr ...)
      `(and (,',name ,$val1 ,$val2)
            (,',name ,$val2 ,@$valN))]
     ))

;;; expr1
;;; -----

(defexpr if
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

(defexpr not
  [(_ $val:expr)
   ($call 'not (list $val))]
  )
; Macro "-" supports both unary and binary usage.
; Macro "+" supports both unary and binary usage.

;;; expr8
;;; -----

(defexpr ref
  [(_ $container:expr $index:expr)
   ($call 'ref (list $container $index))]
  )

(defexpr slice
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

(defexpr slice-until
  [(_ $container:expr $index-to:expr)
   (IVS (E (Q "(")
           $container
           (Q "[")
           (Q ":")
           $index-to
           (Q "]")
           (Q ")")))]
  )

(defexpr slice-from
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

(defexpr ->
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
(defexpr kbd
  [(_ $string:qexpr)
   ($call 'kbd (list $string))]
  )

(defexpr list
  [(_ $val:expr ...)
   (IVS (E (Q "[")
           (apply E (intersperse (Q ",") $val))
           (Q "]")))]
  )

(defexpr dict
  [(_ ($key:expr $val:expr) ...)
   (IVS (E (Q "{")
           (apply IVS
                  (map (cut E
                            <>
                            (Q " ")  ; To parse {s:x} as {(s):x} not {(s:x)}.
                            (Q ":")
                            <>
                            (Q ","))
                       $key
                       $val))
           (Q "}")))]
  [(_ $x:expr ...)
   (define (adjust-key x x:expr)
     (if (keyword? x:expr)
       (keyword->string x:expr)
       x))
   (define (go result xs xs:expr)
     (cond
       [(null? xs)
        (reverse result)]
       [(and (pair? xs) (pair? (cdr xs)))
        (go
          (cons (E (adjust-key (car xs) (car xs:expr))
                   (Q " ")  ; To parse {s:x} as {(s):x} not {(s:x)}.
                   (Q ":")
                   (cadr xs)
                   (Q ","))
                result)
          (cddr xs)
          (cddr xs:expr))]
       [else
         (errorf "Invalid key-value list for dict: ~s" form)]))
   (IVS (E (Q "{")
           (apply IVS (go '() $x $x:expr))
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

(defstmt begin
  [(_ $body:stmt ...)
   (apply IVS $body)]
  )

(defstmt call
  [(_ $application:expr)
   (IVS (S 'call $application))]
  )

(defstmt cond
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

(defstmt define
  ; FIXME: Add tests on failure cases.
  ; FIXME: Detect reassignment.  (run-time? or compile-time?)
  [(_ $var:sym $val:expr)
   (unless (not (func-ctx? ctx))
     (errorf "\"define\" is available only in top-level: ~s" form))
   (IVS (S 'let $var (Q '=) $val))]
  )

(defstmt echo
  [(_ $val:expr ...)
   (IVS (apply S 'echo $val))]
  )

(defstmt function
  ; FIXME: Support !.
  ; FIXME: Support range, abort and dict.
  ; FIXME: Check values on $name and $arg.
  [(_ ($name:qsym $arg:sym ...) $body:qstmt ...)
   (IVS
     (S 'function $name (Q "(") (apply E (intersperse (Q ",") $arg)) (Q ")"))
     (apply IVS (xire-compile-forms $body (make-func-ctx ctx $arg:sym)))
     (S 'endfunction)
     )]
  )

(defstmt for
  [(_ $var:qsym $list:expr $body:qstmt)
   (let1 local-ctx (make-local-ctx ctx (list $var))
     (IVS (S 'for (xire-compile-expr $var local-ctx) 'in $list)
          (xire-compile $body local-ctx)
          (S 'endfor)))]
  [(_ $var:qsym $list:qexpr $body:qstmt ...)
   `(for ,$var ,$list (begin ,@$body))]
  )

(defstmt if
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

(defstmt let
  ; FIXME: Add tests on failure cases.
  [(_ (($var:qsym $val:qexpr) ...) $body:qstmt ...)
   (let ([old-ctx ctx]
         [new-ctx (make-local-ctx ctx $var)])
     `(begin
        ,@(let go ([vars $var]
                   [vals $val]
                   [forms '()])
            (if (null? vars)
              (reverse forms)
              (go (cdr vars)
                  (cdr vals)
                  (cons `(set! ,(transform-value (car vars) #f 'expr new-ctx)
                           ,(transform-value (car vals) #f 'expr old-ctx))
                        forms))))
        ,@(transform-value $body #t 'stmt new-ctx)
        )
     )]
  )

(defstmt let*
  ; FIXME: Add tests on failure cases.
  [(_ (($var:qsym $val:qexpr) ...) $body:qstmt ...)
   (let go ([form `(begin ,@$body)]
            [vars (reverse $var)]
            [vals (reverse $val)])
     (if (null? vars)
       form
       (go `(let ([,(car vars) ,(car vals)])
              ,form)
           (cdr vars)
           (cdr vals))))]
  )

; letrec and letrec* are not useful unless real closures are implemented.

(defstmt return
  [(_ $val:expr)
   (IVS (S 'return $val))]
  )

(defstmt set!
  [(_ $lval:expr $rval:expr)
   (IVS (S 'let $lval (Q '=) $rval))]
  )

(defstmt until
  [(_ $cond:qexpr $body:qstmt ...)
   `(while (not ,$cond) (begin ,@$body))]
  )

(defstmt when
  [(_ $cond:qexpr $then:qstmt ...)
   `(if ,$cond
      (begin
        ,@$then))]
  )

(defstmt while
  [(_ $cond:expr $body:stmt)
   (IVS (S 'while $cond)
        $body
        (S 'endwhile))]
  [(_ $cond:qexpr $body:qstmt ...)
   `(while ,$cond (begin ,@$body))]
  )




;;; __END__
