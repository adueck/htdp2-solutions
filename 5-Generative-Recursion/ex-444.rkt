;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-444) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)
(define (gcd-structural S L)
  (largest-common (divisors S S) (divisors S L)))
 
; N[>= 1] N[>= 1] -> [List-of N]
; computes the divisors of l smaller or equal to k
(check-expect (divisors 10 20) (list 10 5 4 2 1))
(check-expect (divisors 2 4) (list 2 1))
(define (divisors k l)
  (cond
    [(= k 1) (list 1)]
    [else (if (= (remainder l k) 0)
              (cons k (divisors (sub1 k) l))
              (divisors (sub1 k) l))]))
 
; [List-of N] [List-of N] -> N
; finds the largest number common to both k and l
(check-expect (largest-common
               (list 3 8 5 1 9 15)
               (list 2 4 9 22))
              9)
(define (largest-common a b)
  (apply max (common a b)))

; [List-of N] [List-of N] -> [List-of N]
; finds the common elements of two lists
(check-expect (common (list 2 4 8 10)
                      (list 4 10 35))
              (list 4 10))
(define (common a b)
  (filter (lambda (x) (member? x b)) a))

; divisors consumes two numbers because it gives it a range to iterate over
; to find the divisors (max -> 1)
; It consumes S as the first argument in both cases because no divisor can be
; bigger than the smallest of the two numbers