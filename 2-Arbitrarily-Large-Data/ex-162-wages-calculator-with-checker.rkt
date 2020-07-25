;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-162-wages-calculator-with-checker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define PAY 14)

; Number -> Number
; computes the wage for h hours of work
(check-expect (wage 2) (* 2 PAY))
(define (wage h)
  (* PAY h))

; List-of-numbers -> List-of-numbers
; computes the weekly wages for the weekly hours
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (wage 28) '()))
(check-expect (wage* (cons 4 (cons 2 '())))
            (cons (wage 4) (cons (wage 2) '())))
(check-error (wage* (cons 2 (cons 200 '())))
             "impossible amount of hours")
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [(> (first whrs) 100)
     (error "impossible amount of hours")]
    [else
     (cons (wage (first whrs)) (wage* (rest whrs)))]))