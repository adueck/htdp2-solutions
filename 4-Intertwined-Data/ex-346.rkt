;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-346) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; an Add is a structure
;  (make-add N N)
(define-struct add [left right])
; a Mul is a structure
;  (make-mul N N)
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

; a BSL-Value is a Number