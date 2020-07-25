;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-156-shooter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (/ WIDTH 2))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))

; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; A ShotWorld is List-of-numbers. 
; interpretation each number on such a list
;   represents the y-coordinate of a shot

; ShotWorld -> Image
; adds the image of a shot for each  y on w 
; at (MID,y} to the background image
(check-expect (to-image '()) BACKGROUND)
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 9 (cons 20 '())))
              (place-image SHOT XSHOTS 9
                (place-image SHOT XSHOTS 20 BACKGROUND)))
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else
     (place-image
      SHOT XSHOTS (first w)
      (to-image (rest w)))]))

; ShotWorld -> ShotWorld
; moves each shot on w up by one pixel
(check-expect (tock '()) '())
(check-expect (tock (cons 5 '())) (cons 4 '()))
(check-expect (tock (cons 10 (cons 5 '())))
              (cons 9 (cons 4 '())))
(define (tock w)
  (cond
    [(empty? w) '()]
    [else
     (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world 
; if the player presses the space bar
(check-expect (keyh '() "a") '())
(check-expect (keyh '() " ") (cons HEIGHT '()))
(check-expect (keyh (cons 50 '()) "b") (cons 50 '()))
(check-expect (keyh (cons 50 '()) " ")
              (cons HEIGHT (cons 50 '())))
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

(define (main w)
  (big-bang w
   [to-draw to-image]
   [on-tick tock]
   [on-key keyh]))