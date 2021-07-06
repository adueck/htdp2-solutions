;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-316) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)

; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  0)

; Any -> Boolean
; Determines if a x is an Atom
(check-expect (atom? 'add1) #true)
(check-expect (atom? 43) #true)
(check-expect (atom? (list 1 2 3)) #false)
(check-expect (atom? 'hello) #true)
;(define (atom? x)
;  (or
;   (number? x)
;   (string? x)
;   (symbol? x)))
(define (atom? x)
  (not (list? x)))