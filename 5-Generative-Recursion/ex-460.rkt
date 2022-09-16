;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-460) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define EPSILON 0.01)
(define R 100000)
(define (constant x) 20)
(define (linear x) (* 2 x))
(define (square x) (* 3 (sqr x)))
(check-expect (integrate-kepler constant 12 22) 200)
(check-expect (integrate-kepler linear 0 10) 100)
;(check-expect (integrate-kepler square 0 10)
;              (- (expt 10 3) (expt 0 3)))
; [Number -> Number] Number Number -> Number
; Computes the area undef the graph of f between a and b using the Kepler method with trapezoids
(define (integrate-kepler f a b)
  (* (/ 1 2) (- b a) (+ (f a) (f b))))

; This works perfectly for the horizontal line and linear function,
; but it fails by 500 (%50)


(check-within (integrate-rectangles constant 12 22) 200 EPSILON)
(check-within (integrate-rectangles linear 0 10) 100 EPSILON)
(check-within (integrate-rectangles square 0 10)
              (- (expt 10 3) (expt 0 3))
              EPSILON)
; [Number -> Number] Number Number -> Number
; Computes the area undef the graph of f between a and b using the rectangle method
(define (integrate-rectangles f a b)
  (local (; width of one rectangle
          (define W (/ (- b a) R))
          ; midpoint of first rectangle
          (define S (/ W 2))
          (define (area i)
            ; width times height at midpoint (moved right by the width of i rectangles)
            (* W (f (+ (+ a (* i W) S)))))
          (define (summation i)
            (cond
              [(= i 0) 0]
              [else (+ (area i) (summation (sub1 i)))])))
    (summation (sub1 R))))

; The integrate-rectangles is MUCH more accurate for the polynomial equation


; Computes the area undef the graph of f between a and b using the divide and conquer
; strategry and then the Kepler method when the interval is sufficiently small
; [Number -> Number] Number Number -> Number
(check-within (integrate-dc constant 12 22) 200 EPSILON)
(check-within (integrate-dc linear 0 10) 100 EPSILON)
(check-within (integrate-dc square 0 10)
              (- (expt 10 3) (expt 0 3))
              EPSILON)
(define (integrate-dc f a b)
  (local ((define mid (/ (+ a b) 2))
          (define SMALL 0.1)
          (define (handle-half aa bb)
            (if (< (abs (- b a)) SMALL)
                (integrate-kepler f aa bb)
                (integrate-dc f aa bb))))
    (+ (handle-half a mid)
       (handle-half mid b))))