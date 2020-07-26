;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-186-sorted) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers -> List-of-numbers 
; rearranges alon in descending order
 
(check-satisfied (sort> (list 3 2 1)) sorted>?)
(check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-satisfied (sort> (list 12 20 -5)) sorted>?)
 
(define (sort> alon)
  (cond
    [(empty? alon) '()]
    [(cons? alon) (insert
                   (first alon) (sort> (rest alon)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n in the right order in a sorted List-of-numbers
(check-expect (insert 3 '()) (list 3))
(check-expect (insert 3 (list 6 2 1)) (list 6 3 2 1))
(check-expect (insert -4 (list 1 0 -4 -5 -20))
              (list 1 0 -4 -4 -5 -20))
(check-expect (insert 100 (list 3 2 1)) (list 100 3 2 1))
(define (insert n alon)
  (cond
    [(empty? alon) (cons n '())]
    [(cons? alon) (if
                   (>= n (first alon))
                   (cons n alon)
                   (cons
                    (first alon)
                    (insert n (rest alon))))]))

; [cons Number [List-of-numbers]] -> Boolean
; checks to see if a list is sorted in descending order
(check-expect (sorted>? (list 1)) #true)
(check-expect (sorted>? (list 3 2 1)) #true)
(check-expect (sorted>? (list 3 6 1)) #false)
(define (sorted>? alon)
  (cond
    [(empty? (rest alon)) #true]
    [else (and
           (>= (first alon) (second alon))
           (sorted>? (rest alon)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
; test should fail demonstrating it's a bad function
(check-expect (sort>/bad (list 4 1 2)) (list 4 2 1))
; can't really formulate this with check-satisfied,
; because the function does indeed return a list that would
; satisfy sorted>?, it's just not related to the list we passed in
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0))
