;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-071) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels:
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH  400)
(define CENTER (quotient WIDTH 2))

(define-struct game [left-player right-player ball])

(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))

; (game-ball game0)
; selects the ball field of game0
; this yields (make-posn 200 200)
; which was created by passing the CENTER constant
; which was computed by taking (quotient WIDTH 2)

; (posn? (game-ball game0))
; sees if the ball field of game0 is indeed a posn
; it returns #true because it is a posn
; we put in (make-posn CENTER CENTER) for the ball field
; while making game0

; (game-left-player game0)
; selects the left-player field of game0
; this returns 100, which is the value of MIDDLE
; which we created at the beginning with (quotient HEIGHT 2)
