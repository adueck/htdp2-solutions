;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-055) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH  100) ; distances in pixels 
(define HEIGHT  60)
(define YDELTA 3)

(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET) 2))


; An LRCD (short for launching rocket countdown) is one of:
; - "resting"
; a Number between -3 and -1
; - NonnegativeNumber
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting 
; (define (launch x ke)
;   x)
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already 
; (define (fly x)
;   x)

; LRCD -> Image
; renders the image of a rocket at a certain point in the scene
; y is the distance from the top of the scene to the bottom
; of the rocket in pixels
(check-expect (place-rocket 20)
    (place-image ROCKET 10 (- 20 CENTER) BACKG))
(define (place-rocket y)
  (place-image ROCKET 10 (- y CENTER) BACKG))

; LRCD -> Image
; renders the state of a resting, launching, or flying rocket
(check-expect
 (show "resting")
 (place-image ROCKET 10 (- HEIGHT CENTER) BACKG))
 
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 10
              (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)))
 
(check-expect
 (show 53)
 (place-image ROCKET 10 (- 53 CENTER) BACKG))

(define (show x)
  (cond
    [(string? x)
     (place-rocket HEIGHT)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 10
                  (place-rocket HEIGHT))]
    [(>= x 0)
     (place-rocket x)]))

