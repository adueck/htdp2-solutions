;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-457) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define AMOUNT 1)
(define DOUBLED (* AMOUNT 2))
(define EPSILON 0.001)
(require racket/trace)

; An Interest Rate is a decimal number representing a percentage
;  ie. 0.2 means %20 interest

; InterestRate -> Number
; Calculates the number of months it takes for an amount to double given InterestRate r
(check-expect (double-amount-brute-f 0.5) 2)
(check-expect (double-amount-brute-f 0.3) 3)
(define (double-amount-brute-f r)
  (local ((define (double-amount-brute-f* r m)
            (if (>= (calc-amount r m) (* AMOUNT 2))
                m
                (double-amount-brute-f* r (add1 m))))
          (define (calc-amount r m)
            (* AMOUNT (expt (+ 1 r) m))))
    (double-amount-brute-f* r 0)))

; This is regular recursion / brute force - probably not what the book is looking for
; at this stage

; Could do this with binary search/newton's method inspired solution
; if we take the book's info that an interest rate that doubles after 72 months is "small" (epsilon)
; - find midpoint between 0 / 72
; - calculate the value at the midpoint
; - find a new midpoint depending on whether the guess was high or low
; - calculate the value at that new midpoint and re-evaluate
; - stop when its within the EPSILON

; InterestRate -> Number
; Calculates the number of months it takes for an amount to double given InterestRate r
; says how many months it would take to double (within EPSILON)
(check-within (double-am 0.1) 7.277 EPSILON)
; if we round this up we should get the same results as the brute force approach
; (number of complete months it takes to double)
(check-expect (andmap (lambda (r) (= (ceiling (double-am r)) (double-amount-brute-f r)))
                      (list 0.9 0.02 0.1 0.5))
              #true)
(define (double-am r)
  (local ((define (double-am* left right)
            (local ((define mid (/ (+ left right) 2))
                    (define (calc-amount m)
                      (* AMOUNT (expt (+ 1 r) m)))
                    (define amount-at-mid (calc-amount mid)))
              (cond
                [(<= (abs (- amount-at-mid DOUBLED)) EPSILON) mid]
                [(> amount-at-mid DOUBLED) (double-am* left mid)]
                [(< amount-at-mid DOUBLED) (double-am* mid right)]))))
    (double-am* 0 72)))

