;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-456) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define EPSILON 0.001)

; taken from https://gitlab.com/cs-study/htdp/-/blob/main/05-Generative-Recursion/28-Mathematical-Examples/Exercise-456.rkt
; but removed polynomial examples because I don't remember calculus;

;; [Number -> Number] Number -> Number
;; Produces a slope of f at r1.
(check-expect (slope (lambda (x) x) 0) 1)
(check-expect (slope (lambda (x) x) 10) 1)
(check-expect (slope (lambda (x) 5) 3) 0)
(check-expect (slope (lambda (x) (+ (* x 3) 4)) 20) 3)
(check-expect (slope (lambda (x) (+ (* x 3) 4)) -5) 3)
(define (slope f r1)
  (local ((define r0 (- r1 EPSILON))
          (define r2 (+ r1 EPSILON))
          (define f@r0 (f r0))
          (define f@r2 (f r2)))
    (/ (- f@r2 f@r0) (- r2 r0))))

;; [Number -> Number] Number -> Number
;; Produces the root of the tangent through (r1, (f r1)).
(check-expect (root-of-tangent (lambda (i) i) 0) 0)
(check-expect (root-of-tangent (lambda (i) i) 10) 0)
(check-error (root-of-tangent (lambda (i) 10)))
(define (root-of-tangent f r1)
  (local ((define slope-value (slope f r1)))
    (if (zero? slope-value)
        (error "The slope is 0.")
        (- r1 (/ (f r1) slope-value)))))
