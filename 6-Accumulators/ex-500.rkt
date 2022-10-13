;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-500) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] -> Number
; counts the number of items in a list
(check-expect (how-many '()) 0)
(check-expect (how-many '(1 1 1 1)) 4)
(check-expect (how-many '(A B)) 2)
(define (how-many l)
  (cond
    [(empty? l) 0]
    [else (add1 (how-many (rest l)))]))

; [List-of X] -> Number
; counts the number of items in a list
(check-expect (how-many.v2 '()) 0)
(check-expect (how-many.v2 '(1 1 1 1)) 4)
(check-expect (how-many.v2 '(A B)) 2)
(define (how-many.v2 l0)
  (local
    (; [List-of X] Number -> Number
     ; counts the number of items in a list
     ; accumulator a is the amount of items in
     ; l0 to the left of l
     (define (how-many/a l a)
       (cond
         [(empty? l) a]
         [else (how-many/a (rest l) (add1 a))])))
    (how-many/a l0 0)))

; The performance of how-many is O(n) where n is the length of the list.
; Does the accumulator version improve on this?
; The accumulator version improves on the space complexity, making it
; O(1), but does not improve on the time complexity, leaving it at O(n).

; Does the accumulator reduce the amount of space needed to compute the result?
; Yes, it reduces the amount of space to O(1), making it constant for input.