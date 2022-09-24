;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |480|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define SIZE 50)
(define SQ (square SIZE "outline" "black"))
(define Q (circle (* 0.25 SIZE) "solid" "black"))
(define Q-WIDTH (image-width Q))
(define QUEENS 8)

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column
(check-expect (threatening? (make-posn 0 0)
                           (make-posn 1 0))
              #true)
(check-expect (threatening? (make-posn 2 2)
                           (make-posn 1 1))
              #true)
(check-expect (threatening? (make-posn 1 1)
                           (make-posn 0 2))
              #true)
(check-expect (threatening? (make-posn 0 0)
                           (make-posn 2 1))
              #false)
(define (threatening? qp1 qp2)
  (local ((define (hits-x)
            (= (posn-x qp1) (posn-x qp2)))
          (define (hits-y)
            (= (posn-y qp1) (posn-y qp2)))
          (define (hits-diag-up)
            ; slope = 1
            (= (- (posn-y qp2) (posn-y qp1))
               (- (posn-x qp2) (posn-x qp1))))
          (define (hits-diag-down)
            ; slope = -1
            (=
             (- (posn-y qp2) (posn-y qp1))
             (- (- (posn-x qp2) (posn-x qp1))))))
  (or (hits-x)
      (hits-y)
      (hits-diag-up)
      (hits-diag-down))))

; Number -> Image
(define (draw-board n)
  (local ((define ROW (draw-row n))
          (define (drawB m)
            (cond
              [(= m 0) empty-image]
              [else (above ROW (drawB (sub1 m)))])))
    (drawB n)))

; Number -> Image
(define (draw-row n)
  (cond
    [(= n 0) empty-image]
    [else (beside SQ (draw-row (sub1 n)))]))

; QP Image -> Image
(define (place-on p img)
  (local ((define (offset x) (-
                              (- (/ Q-WIDTH 2) (/ SIZE 2))
                              (* SIZE x)))
          (define x-pos (offset (posn-x p)))
          (define y-pos (offset (posn-y p))))
      (overlay/align/offset
       "left" "top" Q
       x-pos
       y-pos
       img)))

; [List-of QP] Number -> Image
(define (render-queens lop n)
  (foldl (lambda (p img) (place-on p img))
         (draw-board n)
         lop))

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
 
(define (n-queens n)
  #false)
