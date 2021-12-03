;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-404) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; like andmap but for two lists of equal length
(check-expect (andmap2 (lambda (x) (> x 5))
                       '(6 9 10)
                       '(50 87 90))
              #true)
(check-expect (andmap2 number?
                       '(6 9 10)
                       '(50 "c" 90))
              #false)
(check-expect (andmap2 string? '() '()) #true)
; solution using abstraction
(define (andmap2 f l1 l2)
  (and
   (andmap f l1)
   (andmap f l2)))
; solution from scratch using recursion
; (define (andmap2 f l1 l2)
;   (cond
;     [(empty? l1) #true]
;     [else (and (f (first l1))
;                (f (first l2))
;                (andmap2 f (rest l1) (rest l2)))]))
