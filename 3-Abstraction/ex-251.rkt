;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum b l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))

; [List-of Number] Number [Number -> Number] -> Number
(check-expect (fold1 '(1 2 3) 0 +) 6)
(check-expect (fold1 '(2 1 5) 1 *) 10)
(define (fold1 l b R)
  (cond
    [(empty? l) b]
    [else
     (R (first l)
        (fold1 (rest l) b R))]))