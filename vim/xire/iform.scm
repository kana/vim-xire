(define-module vim.xire.iform
  (export
    ; Public API.
    pass-final

    ; Not public, but exported to test.
    <pass-final/state>
    bin-op-table
    convert-identifier-conventions
    convert-key-sequence-conventions
    convert-regexp-conventions
    convert-string-conventions
    derive-state
    iform-tag
    iform?
    make-begin
    make-break
    make-call
    make-const
    make-def
    make-ex
    make-for
    make-func
    make-gref
    make-gset
    make-if
    make-let
    make-lref
    make-lset
    make-next
    make-ret
    make-while
    scheme-object->vim-script-notation
    un-op-table
    ))
(select-module vim.xire.iform)

(use util.list)
(use util.match)




;;; The structure of the compiler
;;; =============================
;;;
;;; Xire script is compiled with the following steps:
;;;
;;; Pass 1 (parsing)
;;;     - Xire script is parsed as a list of S expressions.
;;;     - Use of macros are recursively expanded into an intermediate form,
;;;       which is called IForm.
;;;     - This pass returns a list of IForm objects.
;;;       More precisely, a list of <top-stmt> objects.
;;;
;;; Pass Final (code generation)
;;;     - A list of IForm objects is traversed and corresponding Vim script
;;;       is generated.
;;;
;;; More passes will be added between Pass 1 and Pass Final.




;;; Low-level utilities
;;; ===================

(define (scheme-object->vim-script-notation x)
  ; Vim script numbers are 32-bit signed integers.  (cf. :help Number)
  (define MINIMAL-NUMBER (ash -1 31))
  (define MAXIMUM-NUMBER (- (ash 1 31) 1))

  (cond
    [(boolean? x)
     (if x
       1
       0)]
    [(number? x)
     (cond
       [(< x MINIMAL-NUMBER)
        (errorf "Too small number for Vim script: ~s" x)]
       [(< MAXIMUM-NUMBER x)
        (errorf "Too big number for Vim script: ~s" x)]
       [(inexact? x)
        (errorf "Inexact number can not be expressed in Vim script: ~s" x)]
       [(integer? x)
        x]
       ; FIXME: Support floating point numbers in Vim script.
       [else
         (errorf "Invalid number for Vim script: ~s" x)])]
    [(regexp? x)
     (convert-regexp-conventions x)]
    [(string? x)
     (convert-string-conventions x)]
    [(symbol? x)
     (convert-identifier-conventions (symbol->string x))]
    ; List literal is provided by a Xire macro.
    ; Dictionary literal is provided by a Xire macro.
    ; FIXME: How about Funcref?
    [else
      (errorf "Invalid Scheme object for Vim script: ~s" x)]
    )
  )

;; Convert identifier conventions between Scheme and Vim script.  For example,
;; Scheme usually uses hyphen (-) to join words in an identifier, while Vim
;; script uses underscore (_) for that purpose.  This procedure converts such
;; conventions.
(define (convert-identifier-conventions scheme-identifier-string)
  ; FIXME: Raise error for invalid characters as Vim script identifier.
  (regexp-replace-all* scheme-identifier-string
                       #/\?$/ "_p"
                       #/!$/ "_x"
                       #/->/ "_to_"
                       #/[-%]/ "_"
                       ))

;; Convert Scheme regexp object into equivalent one in Vim script.
;; Unlike convert-string-conventions, there is no limitation to write regexp.
;; Because external representation of regexp in Scheme is almost same as one
;; in Vim script.
(define (convert-regexp-conventions scheme-regexp)
  (regexp-replace-all* (regexp->string scheme-regexp)
                       #/'/ "''"
                       #/^(.*)$/ "'\\1'"
                       ))

;; Convert Scheme string object into Vim script string notation.
;;
;; In Xire script, Vim script strings are written as Scheme strings.  But both
;; notations are not completely equivalent.  So that we put the following
;; limitations on Scheme strings which will be compiled into Vim script:
;;
;; (i) In Scheme strings, only the following backslash-escape notations may be
;;     used:
;;
;;     \\ \" \f \n \r \t \uNNNN \xNN \<whitespace>*<newline><whitespace>*
;;
;;     - All but the last notation are the same as Vim script's ones, so that
;;       external representation of Scheme strings and Vim script strings are
;;       the same if this condition is met.  Therefore we can simply write
;;       Scheme strings as if they are Vim script strings.
;;
;;     - The last notation is processed by Scheme's reader and it is simply
;;       ignored.  So that it may be used too.
;;
;;     - Other notations (\0, \UNNNNNNNN) must not be used.  Because:
;;       - There is no equivalent for \UNNNNNNNN.
;;       - Vim script cannot handle NUL character as is.  Though we can write
;;         "\0" in Vim script, such strings are essentially wrong.  So that it
;;         must not be used.
;;
;; (ii) The following backslash-escape notations in Vim script are not
;;      available in Xire script:
;;
;;      (o) \. \.. \... (arbitrary byte, in octal digits)
;;      (x) \x.         (arbitrary byte, in single hex digit)
;;      (X) \X. \X..    (equivalent to \x. and \x..)
;;      (U) \U....      (equivalent to \u....)
;;      (b) \b          (equivalent to \<BS>)
;;      (e) \e          (equivalent to \<Esc>)
;;      (k) \<Key>      (special key sequence)
;;
;;      - (o), (x) and (X): Use "\xNN" instead.
;;      - (b) and (e): Use "\xNN" instead.
;;      - (U): Incompatible with Gauche's "\UNNNNNNNN" notation and it is
;;        rarely used.
;;      - (k): Use (kbd "<Key> ...") form instead.  [KEYSEQ]
;;
;; [KEYSEQ]
;;     In Vim script, it's necessary to write special key sequences in
;;     the following situations:
;;
;;     (A) To denote input:
;;         (a) :execute 'normal!' "\<Key>"
;;         (b) :call feedkeys("\<Key>", 't')
;;         (c) :map <expr> {lhs} "\<Key>"
;;         (d) :map {lhs} <C-r>="\<Key>"<Return>
;;     (B) To compare input:
;;         (a) visualmode() ==# "\<C-v>"
;;         (b) 0 <= index(["\<C-c>", "\<Esc>"], nr2char(getchar()))
;;
;;     But it's possible to write equivalent Vim script if (kbd "<Key> ...")
;;     form is available.  So that it's not a problem even if we cannot write
;;     "\<Key>" directly in Xire script.
(define (convert-string-conventions scheme-string)
  ; FIXME: Check the above limitations are met.
  (format "~s" scheme-string))

;; Translate a key sequence string S into equivalent Vim script string.
;; For example, "ixyx<BS>z" in Scheme string will be translated into
;; "ixyx\<BS>z" in Vim script string.
;;
;; The format of a key sequence string S is almost same as {rhs} for
;; :map-family in Vim script.  Note that we define the following conventions
;; to simplify writing Xire script and the compiler implementation:
;;
;; (a) Don't use ``\'' directly.  Use ``<Bslash>'' instead.  Because ``\'' is
;;     processed by Scheme reader, so that we have to escape ``\'' and such
;;     escaping makes script unreadable.
(define (convert-key-sequence-conventions s)
  (string-append
    "\""
    (regexp-replace-all*
      s
      #/<[^<>]+>/ "\\\\\\0"
      #/"/ "\\\\\""
      )
    "\""))




;;; Intermediate form of Vim script (IForm)
;;; =======================================
;;;
;;; Vim script is not a good programming language, because it is originated
;;; from Ex commands.  Though it might be a good DSL to edit text data, its
;;; syntax is too arbitrary as a programming language.
;;;
;;; So that Xire script is converted into an IForm object at first.
;;; IForm is designed to:
;;;
;;; - Express a code of Vim script in a unified format.
;;; - Manipulate an intermediate code easily.
;;; - Separate passes of the compilation clearly.


;;; Naming conventions
;;; ------------------
;;;
;;; expr                An iform of an expression.
;;; gvar                A global variable.
;;; iform               An iform.
;;; lvar                A local variable.
;;; name                A symbol.
;;; obj                 A non-iform object.
;;; stmt                An iform of a statement.


;;; Utilities
;;; ---------

(define iform?
  ; All iform objects are represented as vectors.
  vector?)

(define (iform-tag iform)
  (vector-ref iform 0))


;;; <top-stmt>
;;; ----------
;;;
;;; <top-stmt> is a <stmt> or one of the following forms:

(define (make-def gvar expr)
  ; Define a global variable.
  `#($DEF ,gvar ,expr))


;;; <stmt>
;;; ------
;;;
;;; <stmt> is one of the following forms:

(define (make-gset gvar expr)
  ; Modify a global variable.
  `#($GSET ,gvar ,expr))

(define (make-let lvars exprs stmt)
  ; Define local variables.
  `#($LET ,lvars ,exprs ,stmt))

(define (make-lset lvar expr)
  ; Modify a local variable.
  `#($LSET ,lvar ,expr))

(define (make-begin stmts)
  ; Execute statements sequentially.
  `#($BEGIN ,stmts))

(define (make-if expr then-stmt else-stmt)
  ; Branch execution.
  `#($IF ,expr ,then-stmt ,else-stmt))

(define (make-while expr stmt)
  ; Loop by a condition.
  `#($WHILE ,expr ,stmt))

(define (make-for lvar expr stmt)
  ; Loop by a list.
  `#($FOR ,lvar ,expr ,stmt))

(define (make-break)
  ; Exit from the most inner loop.
  `#($BREAK))

(define (make-next)
  ; Go to the next step of the most inner loop.
  `#($NEXT))

(define (make-ret expr)
  ; Exit from the current function.
  `#($RET ,expr))

; FIXME: Support :try/:catch/:finally in the distant future.

(define (make-func func-name arg-names stmt)
  ; Create a function.
  `#($FUNC ,func-name ,arg-names ,stmt))

(define (make-ex obj-or-iforms)
  ; Execute an arbitrary Ex command.
  `#($EX ,obj-or-iforms))


;;; <expr>
;;; ------
;;;
;;; <expr> is one of the following forms:

(define (make-const obj)
  ; Return a literal.
  `#($CONST ,obj))

(define (make-gref gvar)
  ; Reference a global variable.
  `#($GREF ,gvar))

(define (make-lref lvar)
  ; Reference a local variable.
  `#($LREF ,lvar))

(define (make-call func-expr-or-op-name arg-exprs)
  ; Call a function or a built-in operator.
  `#($CALL ,func-expr-or-op-name ,arg-exprs))




;;; Pass Final (code generation)
;;; ============================

;;; Misc. utilities
;;; ---------------

(define-constant un-op-table
  '((not . "!")
    (1- . "-")
    (1+ . "+")))

(define (un-op-info op-name)
  (assq op-name un-op-table))

(define (bin-op-info op-name)
  (assq op-name bin-op-table))

(define-constant bin-op-table
  '((or . "||")
    (and . "&&")
    (!= . "!=")
    (!=# . "!=#")
    (!=? . "!=?")
    (!~ . "!~")
    (!~# . "!~#")
    (!~? . "!~?")
    (< . "<")
    (<# . "<#")
    (<= . "<=")
    (<=# . "<=#")
    (<=? . "<=?")
    (<? . "<?")
    (== . "==")
    (==# . "==#")
    (==? . "==?")
    (=~ . "=~")
    (=~# . "=~#")
    (=~? . "=~?")
    (> . ">")
    (># . ">#")
    (>= . ">=")
    (>=# . ">=#")
    (>=? . ">=?")
    (>? . ">?")
    (is . "is")
    (is# . "is#")
    (is? . "is?")
    (isnot . "isnot")
    (isnot# . "isnot#")
    (isnot? . "isnot?")
    (+ . "+")
    (- . "-")
    (.. . ".")
    (* . "*")
    (/ . "/")
    (% . "%")))

(define (bin-op-info op-name)
  (assq op-name bin-op-table))


;;; State for code generation
;;; -------------------------

(define-class <pass-final/state> ()
  ([in-scriptp
     :init-keyword :in-scriptp
     :init-value #t]
   [in-funcp
     :init-keyword :in-funcp
     :init-value #f]
   [func-args
     :init-keyword :func-args
     :init-value '()]  ; Alist of (original-name . new-name).
   [lvars
     :init-keyword :lvars
     :init-value '()]))  ; Alist of (original-name . new-name).

(define (derive-state base-state . args)
  (let1 new-state (make <pass-final/state>)
    (for-each
      (lambda (slot-name)
        (set! (ref new-state slot-name) (ref base-state slot-name)))
      (map slot-definition-name (class-direct-slots <pass-final/state>)))
    (let go ([slot-name (car args)]
             [slot-value (cadr args)]
             [args (cddr args)])
      (set! (ref new-state slot-name) slot-value)
      (unless (null? args)
        (go (car args)
            (cadr args)
            (cddr args))))
    new-state))

(define (make-tmp-name state)
  (cond
    [(ref state 'in-funcp)
      ; Xire script doesn't provide any way to define function-local
      ; variables except "let" family.  And it's not usual to access
      ; function-local variables from other context.  So that it's not
      ; necessary to take care on name collision.
      (gensym "L")]
    [(ref state 'in-scriptp)
     ; There is a chance of name collision between variables explicitly
     ; defined with "define" and variables implicitly defined with "let"
     ; family.  To avoid unexpected name collision, generate variable name
     ; with a prefix which, probably, users will not use.
     (gensym "s:__L")]
    [else
      (error "Lexical variables are not available in this context.")]))

(define (rename-var name state)
  (let1 orig-name&new-name (or (assq name (ref state 'lvars))
                               (assq name (ref state 'func-args)))
    (if orig-name&new-name
      (cdr orig-name&new-name)
      (errorf "Variable is not defined: ~s" name))))


;;; Entry point
;;; -----------

(define (pass-final iforms :optional (state (make <pass-final/state>)))
  (map (cut pass-final/rec <> state)
       iforms))

(define (pass-final/rec iform state)
  (let gen ([iform iform]
            [state state])
    (match iform
      [#('$CONST obj)
        (if (or (boolean? obj) (number? obj) (regexp? obj) (string? obj))
          (scheme-object->vim-script-notation obj)
          (errorf "$CONST contains an invalid object: ~s" obj))]
      [#('$GREF gvar)
        (convert-identifier-conventions
          (symbol->string gvar))]
      [#('$LREF lvar)
        (rename-var lvar state)]
      [#('$CALL (? iform? func-expr) arg-exprs)
        (list (gen func-expr state)
              "("
              (intersperse "," (map (cut gen <> state) arg-exprs))
              ")")]
      [#('$CALL 'if (cond-expr then-expr else-expr))
        (list "("
              (gen cond-expr state)
              " ? "
              (gen then-expr state)
              " : "  ; To parse r?s:t as (r)?(s):(t) not (r)?(s:t).
              (gen else-expr state)
              ")")]
      [#('$CALL (= bin-op-info op) (left-expr right-expr))
        (=> next)
        (unless op
          (next))
        (list "("
              (gen left-expr state)
              " "
              (cdr op)
              " "
              (gen right-expr state)
              ")")]
      [#('$CALL (= un-op-info op) (expr))
        (=> next)
        (unless op
          (next))
        (list "("
              (cdr op)
              (gen expr state)
              ")")]
      [#('$CALL 'ref (collection-expr index-expr))
        (list "("
              (gen collection-expr state)
              "["
              (gen index-expr state)
              "]"
              ")")]
      [#('$CALL 'slice (collection-expr from-expr until-expr))
        (list "("
              (gen collection-expr state)
              "["
              (if from-expr
                (gen from-expr state)
                '())
              " : "  ; To parse l[s:x] as l[(s):x] not l[(s:x)].
              (if until-expr
                (gen until-expr state)
                '())
              "]"
              ")")]
      [#('$CALL '-> (dict-expr name))
        (list "("
              (gen dict-expr state)
              "."
              name
              ")")]
      [#('$CALL 'list (exprs ...))
        (list "["
              (intersperse "," (map (cut gen <> state) exprs))
              "]")]
      [#('$CALL 'dict ((key-exprs ...) (val-exprs ...)))
        (list "{"
              (intersperse
                ","
                (map (lambda (key-expr val-expr)
                       (list (gen key-expr state)
                             " : "  ; To parse {s:x} as {(s):x} not {(s:x)}.
                             (gen val-expr state)))
                     key-exprs
                     val-exprs))
              "}")]
      [#((or '$DEF '$GSET) gvar expr)
        (list "let"
              " "
              (convert-identifier-conventions (symbol->string gvar))
              "="
              (gen expr state)
              "\n")]
      [#('$LSET lvar expr)
        (list "let"
              " "
              (rename-var lvar state)
              "="
              (gen expr state)
              "\n")]
      [#('$LET (lvars ...) (exprs ...) stmt)
        (define new-state
                (derive-state state
                  'lvars (append
                           (map (cut cons <> (make-tmp-name state)) lvars)
                           (ref state 'lvars))))
        (list (map (lambda (lvar expr)
                     (list "let"
                           " "
                           (rename-var lvar new-state)
                           "="
                           (gen expr state)
                           "\n"))
                   lvars
                   exprs)
              (gen stmt new-state))]
      [#('$BEGIN (stmts ...))
        (map (cut gen <> state) stmts)]
      [#('$IF cond-expr then-stmt else-stmt)
        (list "if" " " (gen cond-expr state) "\n"
              (gen then-stmt state)
              "else" "\n"
              (gen else-stmt state)
              "endif" "\n")]
      [#('$WHILE expr stmt)
        (list "while" " " (gen expr state) "\n"
              (gen stmt state)
              "endwhile" "\n")]
      [#('$FOR lvar expr stmt)
        (define new-state
                (derive-state state
                  'lvars (cons
                           (cons lvar (make-tmp-name state))
                           (ref state 'lvars))))
        (list "for"
              " "
              (rename-var lvar new-state)
              " "
              "in"
              " "
              (gen expr state)
              "\n"
              (gen stmt new-state)
              "endfor" "\n")]
      [#('$BREAK)
        '("break" "\n")]
      [#('$NEXT)
        '("continue" "\n")]
      [#('$RET expr)
        (list "return" " " (gen expr state) "\n")]
      [#('$FUNC func-name (arg-names ...) stmt)
        (define (rename arg-name)
          (if (eq? arg-name '...)
            "a:000"
            (string->symbol #`"a:,arg-name")))
        (define new-state
                (derive-state state
                  'in-funcp #t
                  'func-args (append
                               (map (^n (cons n (rename n))) arg-names)
                               (ref state 'func-args))))
        (list "function!"
              " "
              (convert-identifier-conventions (symbol->string func-name))
              "("
              (intersperse
                ","
                (map
                  (lambda (arg-name)
                    (convert-identifier-conventions (symbol->string arg-name)))
                  arg-names))
              ")"
              "\n"
              (gen stmt new-state)
              "endfunction" "\n")]
      [#('$EX (obj-or-iforms ...))
        (letrec ([zap (lambda (x)
                        (cond
                          [(iform? x)
                           (gen x state)]
                          [(pair? x)
                           (cons (zap (car x))
                                 (zap (cdr x)))]
                          [else
                            x]))])
              (list (intersperse " " (map zap obj-or-iforms))
                    "\n"))]
      [else
        (errorf "This iform is not valid: ~s" iform)])))




;;; __END__
