;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-352) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct add [left right])
;; An Add is a structure:
;;    (make-add BSL-var-expr BSL-var-expr)

(define-struct mul [left right])
;; A Mul is a structure:
;;    (make-mul BSL-var-expr BSL-var-expr)

;; A BSL-var-expr is one of:
;; - Number
;; - Symbol
;; - Add
;; - Mul

; BSL-var-exp Symbol N -> BSL-var-exp
(check-expect (subst 'x 'x 3) 3)
(check-expect (subst (make-add 'x 3) 'x 3)
                     (make-add 3 3))
(check-expect (subst
               (make-add (make-mul 'x 'x)
                         (make-mul 'y 'y))
               'x 4)
              (make-add (make-mul 4 4)
                         (make-mul 'y 'y)))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (symbol=? ex x)
                      v
                      ex)]
    [(add? ex) (make-add
                (subst (add-left ex) x v)
                (subst (add-right ex) x v))]
    [(mul? ex) (make-mul
                (subst (mul-left ex) x v)
                (subst (mul-right ex) x v))]))