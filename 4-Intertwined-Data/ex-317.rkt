;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-317) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; S-expr -> Number
; Determinens the depth of an S-expr
(check-expect (depth '(1 2 3)) 1)
(check-expect (depth '(1 (2 3) (3 4 5))) 2)
(check-e
(define (depth sexp)
  (cond
    [(atom? sexp) 1]
    [else (depth-sl sexp)]))

(define (depth-sl sl)
  (cond
    [(empty? sl) 0]
    [else (max
           (depth (first sl))
           (depth-sl (rest sl)))]))


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