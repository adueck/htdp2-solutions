;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-428) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< '(4 1 0 9 2)) '(0 1 2 4 9))
(check-expect (quick-sort< '(5 2 2 1 7)) '(1 2 2 5 7))
(check-expect (quick-sort< '()) '())
(check-expect (quick-sort< '(4)) '(4))
(check-expect (quick-sort< '(1 8 2 3 4 10 1005 7)) '(1 2 3 4 7 8 10 1005))
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(= (length alon) 1) alon]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers (rest alon) pivot))
                    (list pivot)
                    (quick-sort< (largers (rest alon) pivot))))]))
 
; [List-of Number] Number -> [List-of Number]
; produces a list of the numbers that are larger or equal to a given number in a list
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (>= (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
; produces a list of numbers that are smaller than a given number in a list
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

