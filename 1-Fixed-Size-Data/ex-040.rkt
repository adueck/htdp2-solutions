;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-040) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


(define WIDTH-OF-WORLD 200)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))
(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "transparent"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))
(define CAR-BODY
  (rectangle (* WHEEL-RADIUS 10) (* WHEEL-RADIUS 3) "solid" "red"))
(define CAR
  (underlay/align/offset "middle" "bottom"
   CAR-BODY
   0 WHEEL-RADIUS
   BOTH-WHEELS))

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car

; WorldState -> WorldState
; moves the car right by 3 pixels
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))
