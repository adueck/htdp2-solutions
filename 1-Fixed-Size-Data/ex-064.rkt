;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-064) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; it doesn't matter which strategy you follow
; all paths will take the same amount of steps
; for this function I will use the strategy on the left
; where you just walk left x blocks and then up x blocks

; ManhattanPosition is a posn
; interpretation the position in terms of block
; each co-ordinate represents one corner of a city block

; ManhattanPosition -> Number
; shows the distance of a given posn to the origin,
; in terms of number of blocks that need to be walked
(check-expect (manhattan-distance (make-posn 3 0)) 3)
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(define (manhattan-distance p)
  (+ (posn-x p) (posn-y p)))
