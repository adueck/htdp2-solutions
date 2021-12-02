;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-398) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Coefficient is a Number
; A Variable is a Number
;   interpretation: the value to be assigned to an assumed variable
;   of a Coefficient

; A LC (Linear Combination) is a [List-of Coefficient]
; (5 * x)
(define lc1 (list 5))
; (5 * x) + (17 * y)
(define lc2 (list 5 17))
; (5 * x) + (17 + y) + (3 + z)
(define lc3 (list 5 17 3))

; LC [List-of Variable] -> Number
(check-expect (value lc1 (list 10)) 50)
(check-expect (value lc2 (list 10 1)) 67)
(check-expect (value lc3 (list 10 1 2)) 73)
(define (value lc lov)
  (cond
    [(empty? lc) 0]
    [else (+ (* (first lc) (first lov))
             (value (rest lc) (rest lov)))]))