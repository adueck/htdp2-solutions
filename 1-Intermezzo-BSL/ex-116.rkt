;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-116) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;x

; legal because x is an expression

;(= y z)

; legal because = is a primitive, followed by two expressions
; expr can be (primitive expr expr ...)

;(= (= y z) 0)

; legal becaues (= y z) is an expression, as seen above
; still fits the syntax of (primitive expr expr ...)

