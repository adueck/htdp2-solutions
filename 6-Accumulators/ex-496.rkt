;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-496) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; N -> N 
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v1 3) 6)
(define (!.v1 n)
  (cond
    [(zero? n) 1]
    [else (* n (!.v1 (sub1 n)))]))

; N -> N 
; computes (* n (- n 1) (- n 2) ... 1)
(check-expect (!.v2 3) 6) 
(define (!.v2 n0)
  (local (; N ??? -> N
          ; computes (* n (- n 1) (- n 2) ... 1)
          ; accumulator a a is the product of the natural numbers
          ; in the interval [n0,n)
          (define (!/a n a)
            (cond
              [(zero? n) a]
              [else (!/a (sub1 n) (* n a))])))
    (!/a n0 1)))

; Taken from
; https://gitlab.com/cs-study/htdp/-/blob/main/06-Accumulators/32-Designing-Accumulator-Style-Functions/Exercise-497.rkt
(define (evaluate f n0 times)
  (cond
    [(= 0 times) 1]
    [else (+ (f n0) (evaluate f n0 (sub1 times)))]))

; What should the value of a be when n0 is 10 and n is 8?
; (* 10 9 8) == 720

; (time (evaluate !.v1 1000 1000))
; cpu time: 544 real time: 611 gc time: 9
; (time (evaluate !.v2 1000 1000))
; cpu time: 600 real time: 659 gc time: 6