;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-432) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define FIELD-SIZE 20)

; Snake -> Food 
; Creates a new random position for the food which
; was not the previous p and also does not cover the Snake (s)
(check-satisfied (food-create (list (make-posn 1 1))) not=-1-1?)
(define (food-create s)
  (local
    ((define (food-check-create candidate)
       (if (member? candidate s)
           (food-create s)
           candidate)))
    (food-check-create (make-posn (random FIELD-SIZE) (random FIELD-SIZE)))))

; The trivially solvable problem is to create a random position
; then that needs to be combined with a check to see if the position exists in the snake
; if it does exist in the snake the check recursively calls the problem of creating the snake

; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
