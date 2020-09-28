;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-236) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number Lon -> Lon
; adds n to each item on l
(check-expect (addN 5 '()) '())
(check-expect (addN 2 '(1 2 3))
              '(3 4 5))
(check-expect (addN -10 '(100 90))
              '(90 80))
(define (addN n l)
  (cond
    [(empty? l) '()]
    [else
      (cons
       (+ (first l) n)
       (addN n (rest l)))]))

; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* '()) '())
(check-expect (add1* '(2 4 6))
              '(3 5 7))
(define (add1* l)
  (addN 1 l))

; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 '()) '())
(check-expect (plus5 '(2 4 6))
              '(7 9 11))
(define (plus5 l)
  (addN 5 l))

; Lon -> Lon
; subtractis 2 from each item on l
(check-expect (minus2 '()) '())
(check-expect (minus2 '(5 4)) '(3 2))
(define (minus2 l)
  (addN -2 l))
