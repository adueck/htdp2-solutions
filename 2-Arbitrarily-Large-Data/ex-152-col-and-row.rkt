;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-152-col-and-row) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; N Number -> Number
; computes (* n x) without using *

(check-expect (multiply 0 3) 0)
(check-expect (multiply 3 4) 12)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [(positive? n) (+ (multiply (sub1 n) x) x)]))


; How does multiply relate to what you know from grade school?
; It essentially makes the computer add up x n times
