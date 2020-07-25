;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-175-matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Matrix is one of: 
;  – (cons Row '())
;  – (cons Row Matrix)
; constraint all rows in matrix are of the same length
 
; A Row is one of: 
;  – '() 
;  – (cons Number Row)

(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))
(define mat1 (cons row1 (cons row2 '())))

; A
; | 11  12 |
; | 21  22 |

; ATransposed
; | 11  21 |
; | 12  22 |

; Why does transpose ask (empty? (first lln))?
; Because the base-case (the stopping point) of the
; function is when it reaches the end of the first row
; in the matrix

; Matrix -> Matrix
; transposes the given matrix along the diagonal 
 
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))
 
(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; Matrix -> List-of-numbers
(check-expect (first* mat1)
              (cons 11 (cons 21 '())))
(define (first* m)
  (cond
    [(empty? (rest m)) (cons (first (first m)) '())]
    [(cons? m) (cons
                (first (first m)) (first* (rest m)))]))

; Matrix -> Matrix
(check-expect (rest* mat1)
              (cons
               (cons 12 '())
               (cons
                (cons 22 '()) '())))
(check-expect (rest*
               (cons (list 1 2 3 4)
                     (cons (list 4 5 6 7)
                           (cons (list 5 6 7 8) '()))))
              (cons (list 2 3 4)
                    (cons (list 5 6 7)
                          (cons (list 6 7 8) '()))))
(define (rest* m)
  (cond
    [(empty? (rest m)) (cons (rest (first m)) '())]
    [else (cons
           (rest (first m)) ; get the top row minus first col
           (rest* (rest m)) ; add it to the other chopped rows
           )]))

