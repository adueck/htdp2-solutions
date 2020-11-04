;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-256) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] [X -> Number] [NEList-of X] -> X 
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i, 
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(check-expect (argmax add1 (list 2 3 4)) 4)
(define (argmax f lx) ...)

; This function shows the item on the list which is
; the greatest of all the values when the given
; function is applied to each item on th list

; Here would be an analogous purpose statemen for argmin
; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that minimizes f
; if (argmin f (list x-1 ... x-n)) == x-1,
; then (<= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(define (argmin f lx) ...)

