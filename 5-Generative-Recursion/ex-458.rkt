;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-458) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define EPSILON 0.01)
(define R 100000)
(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))
(check-expect (integrate-kepler constant 12 22) 200)
(check-expect (integrate-kepler linear 0 10) 100)
;(check-expect (integrate-kepler square 0 10)
;              (- (expt 10 3) (expt 0 3)))
; Computes the area undef the graph of f between a and b
(define (integrate-kepler f a b)
  (* (/ 1 2) (- b a) (+ (f a) (f b))))

; This works perfectly for the horizontal line and linear function,
; but it fails by 500 (%50)

