;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '(ab bb (43 ("cool" 1) ab)) 'ab 's)
              '(s bb (43 ("cool" 1) s)))
(check-expect (substitute '(b c d) 'f 'g)
              '(b c d))
 
(define (substitute sexp old new)
  (cond
    [(atom? sexp)
     (if (equal? sexp old) new sexp)]
    [else
     (map (lambda (s) (substitute s old new)) sexp)]))

; we had to use lambda for the last simplification because
; the map function only takes one argument, but the
; function we want to feed to it takes 3 arguments
; so we need to wrap it in a lambda function

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