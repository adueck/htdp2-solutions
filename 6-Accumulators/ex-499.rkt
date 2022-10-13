;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-499) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> Number
; Calculates the product of a list of numbers
(check-expect (product '(2 4 1)) 8)
(check-expect (product '(10 -2 3 5)) -300)
(define (product lon)
  (cond
    [(empty? lon) 1]
    [else (* (first lon)
             (product (rest lon)))]))

; [List-of Number] -> Number
; Calculates the product of a list of numbers
(check-expect (product.v2 '(2 4 1)) 8)
(check-expect (product.v2 '(10 -2 3 5)) -300)
(define (product.v2 lon0)
  (local
    (; [List-of Number] Number -> Number
     ; multiplies the numbers in lon together
     ; accumulator a is the value of the numbers
     ; from lon0 to the left of lon, multiplied together
     (define (p/a lon a)
       (cond
         [(empty? lon) a]
         [else
          (p/a (rest lon) (* a (first lon)))])))
    (p/a lon0 1)))

; The performance of product is O(n) where n is the length of the list.
; Does the accumulator version improve on this?
; The accumulator version improves on the space complexity, making it
; O(1), but does not improve on the time complexity, leaving it at O(n)