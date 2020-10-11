;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-244) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define (f x) (x 10))

; (define (name variable variable ...) expr)
; (f x) fits (name variable variable ...)
; (x 10) must be an expr
; an expr can be (name expr expr ...)
; x is a name (of a function value to be passed in)
; 10 is a a number

; The signature of this function would be
; Function -> Number
; (define (f x) (x 10))
; where Function is a function with one Number input

; (define (f x) (x f))

; (define (name variable variable ...) expr)
; (f x) fits
; (x f) fits expr = (name expr expr ...)

; This would be a function that consumes a function
; and returns a function

; Function -> Function
; (define (f x) (x f))
; where Function is a function that takes a function and outputs a function

(define (f x y) (x 'a y 'b))
; (define (name variable variable ...) expr)
; fits (f x y)


; This could be a function that consumes 2 functions
; and returns a list

; FunctionA FunctionB -> Function
; (define (f x y) (x 'a f 'b))

;(define (f x y) (x 'a y 'b))
;(define (fun1 x y z) (y '(x z)))
;(define (fun2 x) x)






