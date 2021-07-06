;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-348) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An And is a structure
;  (make-band Boolean Boolean)
(define-struct band [left right])

; An Or is a structure
;  (make-bor Boolean Boolean)
(define-struct bor [left right])

; A Not is a structure
;  (make-bnot Boolean)
(define-struct bnot [val])

; A BLS-Bool-Expression is one of:
; - #true
; - #false
; - And
; - Or
; - Not

; A BLS-Bool-Value is a Boolean

; BLS-Bool-Expression -> BLS-Bool-Value
; Computes the value of a BLS-Bool-Expression
(check-expect (eval-bool-expression #t)
              #t)
(check-expect (eval-bool-expression #f)
              #f)
(check-expect (eval-bool-expression
               (make-band #t #f))
               #f)
(check-expect (eval-bool-expression
               (make-bor #t #f))
               #t)
(check-expect (eval-bool-expression
               (make-bnot #t))
               #f)
(define (eval-bool-expression ex)
  (cond
    [(boolean? ex) ex]
    [(band? ex) (and (band-left ex) (band-right ex))]
    [(bor? ex) (or (bor-left ex) (bor-right ex))]
    [(bnot? ex) (not (bnot-val ex))]))