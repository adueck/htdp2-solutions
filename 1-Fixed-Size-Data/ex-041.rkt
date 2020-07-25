;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-041) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH-OF-WORLD 200)
(define HEIGHT-OF-WORLD 50)

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
(define Y-CAR (- HEIGHT-OF-WORLD (/ (image-height CAR) 2)))

(define tree
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))
(define BACKGROUND
  (overlay/align
   "middle" "bottom"
   tree
   (empty-scene WIDTH-OF-WORLD HEIGHT-OF-WORLD)))

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the car

; WorldState -> WorldState
; moves the car right by 3 pixels
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)
(define (tock ws)
  (+ ws 3))

; WorldState -> Image
; places the car on the BACKRGROUND SCENE
; according to the given world state
(check-expect (render 50) (place-image CAR 50 Y-CAR BACKGROUND))
(check-expect (render 100) (place-image CAR 100 Y-CAR BACKGROUND))
(check-expect (render 150) (place-image CAR 150 Y-CAR BACKGROUND))
(define (render ws)
   (place-image CAR ws Y-CAR BACKGROUND))

; WorldState -> Boolean
; stop once the car has left the scene
(check-expect (end? WHEEL-RADIUS) false)
(check-expect (end? (+ WIDTH-OF-WORLD (image-width CAR))) true)
(define (end? cw)
  (> cw (+ WIDTH-OF-WORLD (/ (image-width CAR) 2))))

(define (main ws)
  (big-bang ws
    [on-tick tock]
    [to-draw render]
    [stop-when end?]))
