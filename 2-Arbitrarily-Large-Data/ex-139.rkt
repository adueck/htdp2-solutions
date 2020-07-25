;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-139) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-numbers is one of: 
; – '()
; – (cons Number List-of-numbers)

; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)

; List-of-numbers -> Boolean
; checks if all numbers are positive
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 2 '())) #true)
(check-expect (pos? (cons -2 '())) #false)
(check-expect (pos? (cons -2 (cons 2 '()))) #false)
(check-expect (pos? (cons 5 (cons 2 '()))) #true)
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [else (and
           (>= (first lon) 0)
           (pos? (rest lon)))]))

(define MESSAGE "not all numbers are positive")

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

; List-of-numbers -> Number
; adds up a list of positive numbers
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 2 '())) 2)
(check-error (checked-sum (cons -2 '())) MESSAGE)
(check-error (checked-sum (cons -2 (cons 2 '()))) MESSAGE)
(check-expect (checked-sum (cons 5 (cons 2 '()))) 7)
(define (checked-sum lon)
  (cond
    [(pos? lon) (sum lon)]
    [else (error MESSAGE)]))