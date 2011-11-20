(define-module vim.xire.ivs
  (export
    ; Public API.
    <ivs>
    E
    IVS
    Q
    S

    ; Not public, but exported to test.
    ))
(select-module vim.xire.ivs)

(use text.tree)
(use util.list)
(use vim.xire.iform)  ; FIXME: A temporary stuff for the IVS-IForm migration.




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




;;; __END__
