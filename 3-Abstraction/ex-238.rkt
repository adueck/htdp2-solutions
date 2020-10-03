;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf '(3 5 1 9)) 1)
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup '(3 5 1 9)) 9)
(check-expect (sup '(9 2 3 4)) 9)
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Nelon R -> Number
; determines the appropriate number based on R comparison
(check-expect (most > '(2 8 1)) 8)
(check-expect (most < '(2 8 1)) 1)
(define (most R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (R (first l)
            (most R (rest l)))
         (first l)
         (most R (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-1 '(2 1 4)) 1)
;(check-expect
; (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
;              12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(define (inf-1 l)
  (most < l))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-1 '(2 1 4)) 4)
;(check-expect
; (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
;              17 18 19 20 21 22 23 24 25)) 25)
(define (sup-1 l)
  (most > l))

; Using these with long lists is VERY slow because with each
; recursive iteration you need to calculate the result once for
; the comparison, and then again if the first was not the most R.
; Because of recursion this increases the compute time exponentially.

; Now we will modify the original functions using min and max to
; compute the answer in a more effecient way.

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-min '(3 5 1 9)) 1)
(define (inf-min l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l) (inf-min (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-max '(3 5 1 9)) 9)
(check-expect (sup-max '(9 2 3 4)) 9)
(define (sup-max l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max (first l) (sup-max (rest l)))]))

; Abstracting again we get

; A Comparator is one of the following functions:
; - min
; - max

; Nelon Comparator -> Number
; determines the C number on l
(check-expect (most-comp min '(3 1 5)) 1)
(check-expect (most-comp max '(3 1 5)) 5)
(define (most-comp C l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (C (first l) (most-comp C (rest l)))]))

; Nelon -> Number
; determines the smallest 
; number on l (optimized)
(check-expect
 (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
              12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(check-expect
 (inf-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
              17 18 19 20 21 22 23 24 25)) 1)
(check-expect (inf-2 '(3 5 1 9)) 1)
(define (inf-2 l)
  (most-comp min l))

; Nelon -> Number
; determines the largest
; number on l (optimized)
(check-expect
 (sup-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
              12 11 10 9 8 7 6 5 4 3 2 1)) 25)
(check-expect
 (sup-2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
              17 18 19 20 21 22 23 24 25)) 25)
(check-expect (sup-2 '(3 5 1 9)) 9)
(define (sup-2 l)
  (most-comp max l))

; These versions are so much faster because there is only one
; process of calculation needed for the min/max comparison
; and then the actual number value.
 
