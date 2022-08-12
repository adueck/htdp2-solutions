;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-440) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)
(define (gcd-structural n m)
  (local (; N -> N
          ; determines the gcd of n and m less than i
          (define (greatest-divisor-<= i)
            (cond
              [(= i 1) 1]
              [else
               (if (= (remainder n i) (remainder m i) 0)
                   i
                   (greatest-divisor-<= (- i 1)))])))
    (greatest-divisor-<= (min n m))))

; gcd-structural works by starting with the smallest of the two numbers
; and then trying to divide both numbers by that number and seeing if that division
; works with no remainder. If not, it drops down one number and tries again,
; doing this until it finds one number that both can be divided by without leaving
; a remainder


; N[>= 1] N[>= 1] -> N
; Finding the gcd using Euclid's Algorithm
(check-expect (gcd-euc 6 25) 1)
(check-expect (gcd-euc 18 24) 6)
(define (gcd-euc n m)
  (cond
    [(= 0 n) m]
    [(= 0 m) n]
    [else (local
            ((define l (max n m))
             (define s (min n m)))
            (gcd-euc s (remainder l s)))]))

(check-expect (gcd-generative 6 25) 1)
(check-expect (gcd-generative 18 24) 6)
(define (gcd-generative n m)
  (local (; N[>= 1] N[>=1] -> N
          ; generative recursion
          ; (gcd L S) == (gcd S (remainder L S)) 
          (define (clever-gcd L S)
            (cond
              [(= S 0) L]
              [else (clever-gcd S (remainder L S))])))
    (clever-gcd (max m n) (min m n))))

; Euclid's algorithm drawn out from the template
(define-struct pair [a b])
(check-expect (general (make-pair 6 25)) 1)
(check-expect (general (make-pair 18 24)) 6)
(define (general p)
  (cond
    [(trivial? p) (solve p)]
    [else
     (general
       (generate p))]))

; Pair -> Boolean
(define (trivial? p)
  (or (= 0 (pair-a p)) (= 0 (pair-b p))))

; Pair -> Number
(define (solve p)
  (if (= 0 (pair-a p)) (pair-b p) (pair-a p)))

; Pair -> Pair
(define (generate p)
  (local
    ((define s (exfp p min))
     (define l (exfp p max)))
    (make-pair s (remainder l s))))

(define (exfp p f)
  (f (pair-a p) (pair-b p)))

; Testing results
; (time (gcd 101135853 45014640))
; cpu time for each
; gcd-structural
; 8991
; gcd-euc
; 0
; gcd-generative
; 0
; general
; 0
; gcd
; 0