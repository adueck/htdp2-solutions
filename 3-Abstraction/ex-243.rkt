;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-243) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f x) x)

(cons f '())

; f is i value, the first item in the list

; (f f)
; f is a value, which contains the function to be run
; the second f is also a value, passed into the argument of
; the function

; (cons f (cons 10 (cons (f 10) '())))
; f is a value, a function which is first item on the list
; 10 is a vaule, a number which is the second item on the list
; (f 10) is an expression, which evaluates to 10