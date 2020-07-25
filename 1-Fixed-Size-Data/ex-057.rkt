;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-057) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
; bottom of the canvas and the rocket (its height)

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)
(define (launch x ke)
 (cond
   [(string? x) (if (string=? " " ke) -3 x)]
   [(<= -3 x -1) x]
   [(>= x 0) x]))
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) 0)
(check-expect (fly 10) (+ 10 YDELTA))
(check-expect (fly 22) (+ 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (+ x 1)]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> Boolean
; evaluates true when it's time to stop the program
; which is when the height (distance from top) reaches 0
(check-expect (end? "resting") #false)
(check-expect (end? -2) #false)
(check-expect (end? 20) #false)
(check-expect (end? HEIGHT) #false)
(check-expect (end? (add1 HEIGHT)) #true)
(define (end? x)
  (cond
    [(string? x) #false]
    [(<= -3 x -1) #false]
    [(<= x HEIGHT) #false]
    [(> x HEIGHT) #true]))

; LRCD -> Image
; renders the image of a rocket at a certain point in the scene
; y is the distance from the bottom of the scene to the bottom
; of the rocket in pixels
(check-expect (place-rocket 20)
    (place-image ROCKET 10 (- HEIGHT CENTER 20) BACKG))
(define (place-rocket y)
  (place-image ROCKET 10 (- HEIGHT CENTER y) BACKG))

; LRCD -> Image
; renders the state of a resting, launching, or flying rocket
(check-expect
 (show "resting") (place-rocket 0))
 
(check-expect
 (show -2)
 (place-image (text "-2" 20 "red")
              10 10
              (place-rocket 0)))
 
(check-expect
 (show 53) (place-rocket 53))

(define (show x)
  (cond
    [(string? x)
     (place-rocket 0)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 10
                  (place-rocket 0))]
    [(>= x 0)
     (place-rocket x)]))

; LRCD -> LRCD
(define (main2 s)
  (big-bang s
    [on-tick fly 1/3] ; clock ticks every second
    [stop-when end?]
    [to-draw show]
    [on-key launch]))
