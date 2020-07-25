;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-150-add-to-pi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N -> Number
; computes (+ n pi) without using +

(check-within (add-to-pi 3) (+ 3 pi) 0.001)
(check-within (add-to-pi 0) (+ 0 pi) 0.001)
; check-within is used because we pi is not a natural number
; goes to infinite decimal places, so we need to set a range

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [(positive? n) (add1 (add-to-pi (sub1 n)))]))

; N Number -> Number
; computes (+ n x) without using +
(check-expect (add-to-x 2 3) 5)
(check-expect (add-to-x 0 4) 4)

(define (add-to-x n x)
  (cond
    [(zero? n) x]
    [(positive? n) (add1 (add-to-x (sub1 n) x))]))