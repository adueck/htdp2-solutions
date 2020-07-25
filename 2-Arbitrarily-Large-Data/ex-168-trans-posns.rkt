;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-168-trans-posns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of
; - '()
; - (cons posn List-of-posns)
'()
(cons (make-posn 2 3) '())

; List-of-posns -> Number
; produces the sum of all the x-coordinates in a list
; of positions
(check-expect (sum '()) 0)
(check-expect (sum
               (cons (make-posn 3 5)
                     (cons (make-posn 2 1) '())))
              5)
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (+
      (posn-x (first lop))
      (sum (rest lop)))]))
