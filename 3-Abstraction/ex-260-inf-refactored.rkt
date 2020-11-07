;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-260-inf-refactored) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf '(3 5 1 9)) 1)
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local ((define inf-in-rest (inf (rest l))))
       (if (< (first l) inf-in-rest)
           (first l)
           inf-in-rest))]))

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
     (local ((define sup-in-rest (sup (rest l))))
       (if (> (first l) sup-in-rest)
           (first l)
           sup-in-rest))]))

; Nelon R -> Number
; determines the appropriate number based on R comparison
(check-expect (most > '(2 8 1)) 8)
(check-expect (most < '(2 8 1)) 1)
(define (most R l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (local ((define most-in-rest (most R (rest l))))
       (if (R (first l) most-in-rest)
           (first l)
           most-in-rest))]))

; Nelon -> Number
; determines the smallest 
; number on l
(check-expect (inf-1 '(2 1 4)) 1)
(check-expect
 (inf-1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
              12 11 10 9 8 7 6 5 4 3 2 1)) 1)
(define (inf-1 l)
  (most < l))

; Nelon -> Number
; determines the largest 
; number on l
(check-expect (sup-1 '(2 1 4)) 4)
(check-expect
 (sup-1 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
              17 18 19 20 21 22 23 24 25)) 25)
(define (sup-1 l)
  (most > l))

; All these tests indeed run much faster once we saved the answer
; to the recursive call with local
