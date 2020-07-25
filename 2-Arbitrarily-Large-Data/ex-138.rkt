;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-138) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)


; List-of-amounts -> Number
; produces the sum of a list of amounts
(check-expect (sum '()) 0)
(check-expect (sum (cons 1 '())) 1)
(check-expect (sum (cons 3 (cons 2 '()))) 5)
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [else (+ (first loa) (sum (rest loa)))]))
(sum (cons 3 (cons 2 '())))