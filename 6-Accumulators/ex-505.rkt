;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-505) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; N [>=1] -> Boolean
; determines whether n is a prime number
(check-expect (is-prime? 3) #t)
(check-expect (is-prime? 11) #t)
(check-expect (is-prime? 6) #f)
(check-expect (is-prime? 1) #t)
(check-expect (is-prime? 2) #f)
(define (is-prime? n)
  (cond
    [(= n 1) #t]
    [(= n 2) #f]
    [else
     (local
      (; Number Number -> Boolean
       ; accumulator d the divisor increasingly smaller than n
       (define (is-prime?/a d)
         (cond
           [(= 1 d) #t]
           [else (and
                  (not (= 0 (modulo n d)))
                  (is-prime?/a (sub1 d)))])))
       (is-prime?/a (sub1 n)))]))
