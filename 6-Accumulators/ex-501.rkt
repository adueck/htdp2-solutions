;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-501) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]))

; N -> Number 
; adds n to pi without using +
(check-within (add-to-pi.v2 2) (+ 2 pi) 0.001)
(define (add-to-pi.v2 n)
  (local
    (; N Number -> Number
     ; accumulator a is the number which will be added
     ; to with the integer value n
     (define (add-to/a n a)
       (cond
         [(zero? n) a]
         [else (add-to/a (sub1 n) (add1 a))])))
    (add-to/a n pi)))
