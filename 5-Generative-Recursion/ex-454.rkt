;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-454) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Matrix is a [List-of [List-of Number]]

; Creates a matrix from a list of numbers that is n2 long
; generative: cons's the first line onto the rest of the numbers
; with the first line removed
; Number [List-of Number] -> Matrix
(check-expect
  (create-matrix 2 (list 1 2 3 4))
  (list (list 1 2)
        (list 3 4)))
(check-expect
 (create-matrix 3 (list 1 2 3 4 5 6 7 8 9))
 (list (list 1 2 3)
       (list 4 5 6)
       (list 7 8 9)))
(define (create-matrix n l)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (first-row n l)
      (create-matrix n (remove-first-row n l)))]))

; Number [List-of Number] -> [List-of Number]
(check-expect (first-row 3 (list 1 2 3 4 5))
              (list 1 2 3))
(check-expect (first-row 5 '()) '())
(define (first-row n l)
  (cond
   [(empty? l) '()]
   [(= 0 n) '()]
   [else (cons
          (first l)
          (first-row (sub1 n) (rest l)))]))

; Number [List-of-Number] -> [List-of Number]
(check-expect (remove-first-row 3 (list 1 2 3 4 5))
              (list 4 5))
(check-expect (remove-first-row 1 (list 1 2 3 4 5))
              (list 2 3 4 5))
(define (remove-first-row n l)
  (cond
    [(empty? l) '()]
    [(= 0 n) l]
    [else (remove-first-row (sub1 n) (rest l))]))