;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-347) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; an Add is a structure
;  (make-add BSL-Expr BSL-Expr)
(define-struct add [left right])
; a Mul is a structure
;  (make-mul BSL-Expr BSL-Expr)
(define-struct mul [left right])


(make-add 10 -10)
(make-add
 (make-mul 20 3)
 33)
(make-add
 (make-mul 3.14
           (make-mul 2 3))
 (make-mul 3.24
           (make-mul -1 -9)))

(+ -1 2)
(+ (* -2 -3) 33)
(* (+ 1 (* 2 3)) 3.14)

; a BSL-Expression is one of
; - Number
; - Add
; - Mul

; a BSL-Value is a Number

; BSL-Expression -> BSL-Value
; Evaluates a BSL-Explession and outputs it's value
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression
               (make-add 1 1)) 2)
(check-expect (eval-expression
               (make-mul 3 10)) 30)
(check-expect (eval-expression
               (make-add (make-mul 1 1) 10))
               11)
(define (eval-expression ex)
  (cond
    [(number? ex) ex]
    [(add? ex) (+
                (eval-expression (add-left ex))
                (eval-expression (add-right ex)))]
    [(mul? ex) (*
                (eval-expression (mul-left ex))
                (eval-expression (mul-right ex)))]))