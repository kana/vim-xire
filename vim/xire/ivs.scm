(define-module vim.xire.ivs
  (export
    ; Public API.
    <ivs>
    E
    IVS
    Q
    S
    scheme-object->vim-script-notation

    ; Not public, but exported to test.
    convert-identifier-conventions
    convert-regexp-conventions
    convert-string-conventions
    ))
(select-module vim.xire.ivs)

(use text.tree)
(use util.list)




;;; Constructors for IVS
;;; ====================

(define-class <ivs> ()
  ([nodes
     :init-keyword :nodes]
   [linedp
     :init-keyword :lined]
   [quotedp
     :init-keyword :quotedp]
   [spacedp
     :init-keyword :spacedp]))

(define-method object-equal? ((a <ivs>) (b <ivs>))
  (and (equal? (ref a 'linedp) (ref b 'linedp))
       (equal? (ref a 'quotedp) (ref b 'quotedp))
       (equal? (ref a 'spacedp) (ref b 'spacedp))
       (every equal? (ref a 'nodes) (ref b 'nodes))))


(define (IVS . nodes)
  (unless (every (cut is-a? <> <ivs>) nodes)
    (errorf "IVS takes only IVS objects, but given: ~s" nodes))
  (make <ivs>
        :nodes nodes
        :lined #f
        :quotedp #f
        :spacedp #f))

(define (S . nodes)
  (make <ivs>
        :nodes nodes
        :lined #t
        :quotedp #f
        :spacedp #t))

(define (E . nodes)
  (make <ivs>
        :nodes nodes
        :lined #f
        :quotedp #f
        :spacedp #f))

(define (Q . atoms)
  (make <ivs>
        :nodes atoms
        :lined #f
        :quotedp #t
        :spacedp #f))




;;; Translation API from IVS to Vim script
;;; ======================================

(define-method write-tree ((tree <ivs>) out)
  (define (writer x out)
    (cond
      [(ref tree 'quotedp)
       (display x out)]
      [(is-a? x <ivs>)
       (write-tree x out)]
      [else
       (display (scheme-object->vim-script-notation x) out)]))
  (map (cut writer <> out)
       (if (ref tree 'spacedp)
         (intersperse (Q " ") (ref tree 'nodes))
         (ref tree 'nodes)))
  (when (ref tree 'linedp)
    (newline out)))

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
    ; List literal is provided by a xire macro.
    ; Dictionary literal is provided by a xire macro.
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
;; In xire script, Vim script strings are written as Scheme strings.  But both
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
;;      available in xire script:
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
;;     "\<Key>" directly in xire script.
(define (convert-string-conventions scheme-string)
  ; FIXME: Check the above limitations are met.
  (format "~s" scheme-string))




;;; __END__
