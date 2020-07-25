;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-068) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; flat representation of ball data
(define-struct ballf [x y deltax deltay])

; nested representation of ball data
;(define-struct vel [deltax deltay])
;(define-struct ball [location velocity])
;(define ball1
;  (make-ball (make-posn 30 40) (make-vel -10 5)))

; Creating an instance that has the same interpretation
; as ball1, but with a flat representation
(define ball1-f
  (make-ballf 30 40 -10 5))
