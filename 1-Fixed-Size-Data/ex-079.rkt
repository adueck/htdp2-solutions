;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-079) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Color is one of:
; — "white"
; — "yellow"
; — "orange"
; — "green"
; — "red"
; — "blue"
; — "black"

(define c1 "white")
(define c2 "red")


; H is a Number between 0 and 100.
; interpretation represents a happiness value

(define h1 23)
(define h2 72)

(define-struct person [fstname lstname male?])
; A Person is a structure:
;   (make-person String String Boolean)

(define p1 (make-person "Bill" "Smith" #true))
(define p2 (make-person "Mary" "Jones" #false))

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person String PositiveInteger H)
; interpretation the Person who owns the dog,
; the name of the dog, the age of the dog, and
; the H (happiness level) of the dog
(define d1 (make-dog
            p1
            "Puddles"
            4
            h1))
(define d2 (make-dog
            (make-person "Nancy" "White" #false)
            "Puddles"
            4
            88))

; A Weapon is one of:
; - #false
; - Posn
; interpretation #false means the missile hasn't
; been fired yet; a Posn means it is in flight

(define w1 #false)
(define w2 (make-posn 10 26))