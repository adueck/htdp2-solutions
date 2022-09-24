;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |478|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define SIZE 50)
(define SQ (square SIZE "outline" "black"))
(define QUEEN (circle (* 0.25 SIZE) "solid" "black"))
(define Q-WIDTH (image-width QUEEN))

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

; Posn Image -> Image
(define (place-on p img)
  (local ((define (offset x) (-
                              (- (/ Q-WIDTH 2) (/ SIZE 2))
                              (* SIZE x))))
    (overlay/align/offset
     "left" "top" QUEEN
     (offset (posn-x p))
     (offset (posn-y p))
     img)))

; [List-of Posn] Number -> Image
(define (draw-setup lop n)
  (foldl (lambda (p img) (place-on p img))
         (draw-board n)
         lop))

(draw-setup (list (make-posn 0 0)
                  (make-posn 2 1))
            3)

; You can also place the first queen in all squares of the top-most row,
; the right-most column, and the bottom-most row. Explain why all of these
; solutions are just like the three scenarios depicted in figure 173

; With each of those placements it leaves 2 possible spaces for the next placement

; If you place a queen on the central space it is not possible to place anything else on a 3x3 board