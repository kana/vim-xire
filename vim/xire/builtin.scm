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

(define-macro (define-binary-operator name :optional default-val)
  `(defexpr ,name
     [(_ $val1:qexpr)
      (when (undefined? ,default-val)
        (errorf "Operator ~s takes two or more arguments" ',name))
      `(,',name ,',default-val ,$val1)]
     [(_ $val1:expr $val2:expr)
      ($call ',name (list $val1 $val2))]
     [(_ $val1:qexpr $val2:qexpr $valN:qexpr ...)
      `(,',name (,',name ,$val1 ,$val2)
                ,@$valN)]
     ))

(define-macro (define-comparison-operator name)
  `(defexpr ,name
     [(_ $val1:expr $val2:expr)
      ($call ',name (list $val1 $val2))]
     [(_ $val1:qexpr $val2:qexpr $valN:qexpr ...)
      `(and (,',name ,$val1 ,$val2)
            (,',name ,$val2 ,@$valN))]
     ))

;;; expr1
;;; -----

(defexpr if
  [(_ $cond:expr $then:expr $else:expr)
   ($call 'if (list $cond $then $else))]
  )

;;; expr2
;;; -----

(define-binary-operator or)

;;; expr3
;;; -----

(define-binary-operator and)

;;; expr4
;;; -----

(define-comparison-operator !=)
(define-comparison-operator !=#)
(define-comparison-operator !=?)
(define-comparison-operator !~)
(define-comparison-operator !~#)
(define-comparison-operator !~?)
(define-comparison-operator <)
(define-comparison-operator <#)
(define-comparison-operator <=)
(define-comparison-operator <=#)
(define-comparison-operator <=?)
(define-comparison-operator <?)
(define-comparison-operator ==)
(define-comparison-operator ==#)
(define-comparison-operator ==?)
(define-comparison-operator =~)
(define-comparison-operator =~#)
(define-comparison-operator =~?)
(define-comparison-operator >)
(define-comparison-operator >#)
(define-comparison-operator >=)
(define-comparison-operator >=#)
(define-comparison-operator >=?)
(define-comparison-operator >?)
(define-comparison-operator is)
(define-comparison-operator is#)
(define-comparison-operator is?)
(define-comparison-operator isnot)
(define-comparison-operator isnot#)
(define-comparison-operator isnot?)

;;; expr5
;;; -----

(define-binary-operator + 0)
(define-binary-operator - 0)
(define-binary-operator ..)

;;; expr6
;;; -----

(define-binary-operator *)
(define-binary-operator /)
(define-binary-operator %)

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
   ($call 'slice (list $container $index-from $index-to))]
  )

(defexpr slice-until
  [(_ $container:expr $index-to:expr)
   ($call 'slice (list $container #f $index-to))]
  )

(defexpr slice-from
  [(_ $container:expr $index-from:expr)
   ($call 'slice (list $container $index-from #f))]
  )

(defexpr ->
  [(_ $dict:expr $name:sym)
   ($call '-> (list $dict $name))]
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
  [(_ $vals:expr ...)
   ($call 'list $vals)]
  )

(defexpr dict
  [(_ ($keys:expr $vals:expr) ...)
   ($call 'dict (list $keys $vals))]
  [(_ $xs:qexpr ...)
   (define (adjust-key x)
     (if (keyword? x)
       (keyword->string x)
       x))
   (define (go keys vals xs)
     (cond
       [(null? xs)
        (values
          (reverse keys)
          (reverse vals))]
       [(and (pair? xs) (pair? (cdr xs)))
        (go  (cons (transform-value (adjust-key (car xs)) #f 'expr ctx) keys)
             (cons (transform-value (cadr xs) #f 'expr ctx) vals)
             (cddr xs))]
       [else
         (errorf "Invalid key-value list for dict: ~s" form)]))
   ($call 'dict (call-with-values (lambda () (go '() '() $xs)) list))]
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
   ($begin $body)]
  )

(defstmt call
  [(_ $application:expr)
   ($ex (list 'call $application))]
  )

(defstmt cond
  [(_ [$conds:expr $thens:stmt] ...)
   (let go ([conds:expr $conds:expr]
            [conds $conds]
            [thens $thens]
            [stmts '()])
     (cond
       [(null? conds:expr)
        (if (null? stmts)
          ($begin '())
          ($begin (reverse (cons ($ex '(endif)) stmts))))]
       [else
         (go (cdr conds:expr)
             (cdr conds)
             (cdr thens)
             (cons (car thens)
                   (cons ($ex (list (if (null? stmts)
                                      'if
                                      'elseif)
                                    (if (and (null? (cdr conds:expr))
                                             (eq? (car conds:expr) 'else))
                                      ($const #t)
                                      (car conds))))
                         stmts)))]))]
  )

(defstmt define
  ; FIXME: Add tests on failure cases.
  ; FIXME: Detect reassignment.  (run-time? or compile-time?)
  [(_ $var:qsym $val:expr)
   (unless (not (func-ctx? ctx))
     (errorf "\"define\" is available only in top-level: ~s" form))
   ($def $var $val)]
  )

(defstmt echo
  [(_ $vals:expr ...)
   ($ex (list* 'echo $vals))]
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
  [(_ $name:qsym $list:expr $body:qstmt)
   (let* ([old-ctx ctx]
          [lvars (make-lvars (list $name) (list (undefined)) old-ctx)]
          [new-ctx (make-local-ctx~ ctx lvars)])
     ($for (car lvars)
           $list
           (transform-value $body #f 'stmt new-ctx)))]
  [(_ $var:qsym $list:qexpr $body:qstmt ...)
   `(for ,$var ,$list (begin ,@$body))]
  )

(defstmt if
  [(_ $cond:expr $then:stmt)
   ($if $cond $then ($begin '()))]
  [(_ $cond:expr $then:stmt $else:stmt)
   ($if $cond $then $else)]
  )

(defstmt let
  ; FIXME: Add tests on failure cases.
  [(_ (($names:qsym $vals:expr) ...) $body:qstmt ...)
   (let* ([old-ctx ctx]
          [lvars (make-lvars $names $vals old-ctx)]
          [new-ctx (make-local-ctx~ ctx lvars)])
     ($let lvars
           (transform-value `(begin ,@$body) #f 'stmt new-ctx)))]
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
   ($ret $val)]
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
   ($begin
     (list
       ($ex (list 'while $cond))
       $body
       ($ex (list 'endwhile))))]
  [(_ $cond:qexpr $body:qstmt ...)
   `(while ,$cond (begin ,@$body))]
  )




;;; __END__
