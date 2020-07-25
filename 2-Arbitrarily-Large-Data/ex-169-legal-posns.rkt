;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-169-legal-posns) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-posns is one of
; - '()
; - (cons posn List-of-posns)
'()
(cons (make-posn 2 3) '())

; List-of-posns -> List-of-posn
; returns a list of positions with the y coordinates
; increased by one
(check-expect (translate '()) '())
(check-expect (translate
               (cons (make-posn 2 3)
                     (cons (make-posn 3 5) '())))
              (cons (make-posn 2 4)
                    (cons (make-posn 3 6) '())))
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop)
     (cons
      (move-y (first lop)) (translate (rest lop)))]))

; posn -> posn
; translates a position by increasing y coordinate by 1
(check-expect (move-y (make-posn 2 3)) (make-posn 2 4))
(define (move-y p)
  (make-posn (posn-x p) (+ (posn-y p) 1)))
