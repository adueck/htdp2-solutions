;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-250) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))

	
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; Number [Number -> Number] -> [List-of Number]
(define (tabulate n R)
  (cond
    [(= n 0) (list (R 0))]
    [else
     (cons
      (R n)
      (tabulate (sub1 n) R))]))

; Number -> [List-of Number]
(check-expect (tabulate-sqr 3)
              (list (sqr 3) (sqr 2) (sqr 1) (sqr 0)))
(define (tabulate-sqr n)
  (tabulate n sqr))

; Number -> [List-of Number]
; DON'T KNOW HOW TO TEST WITH INEXACT NUMBERS HERE :-/
(define (tabulate-tan n)
  (tabulate n tan))