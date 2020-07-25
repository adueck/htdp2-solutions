;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-095) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define ASPECT-RATIO (/ 9 16))
(define WIDTH 400)
(define HEIGHT (* WIDTH ASPECT-RATIO))
(define BACKGROUND
  (above (empty-scene WIDTH (* HEIGHT 0.8) "blue")
         (empty-scene WIDTH (* HEIGHT 0.2) "green")))

(define TANK-HEIGHT (* WIDTH 0.08))
(define TANK (rectangle
              (* WIDTH 0.18)
              TANK-HEIGHT
              "solid" "brown"))

(define UFO (ellipse
             (* WIDTH 0.18)
             (* WIDTH 0.08)
             "solid" "grey"))

(define DEMO-SCENE
  (place-images/align
   (list TANK UFO)
   (list (make-posn (* WIDTH 0.3) HEIGHT)
         (make-posn (* WIDTH 0.6) (* HEIGHT 0.2)))
   "center" "bottom"
   BACKGROUND))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)


(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; (make-aim (make-posn 20 10) (make-tank 28 -3))
; This is an example of a SIGS based on the first clause of the
; data definition. This is becasue it only contains a UFO
; and a Tank

;(make-fired (make-posn 20 10)
;            (make-tank 28 -3)
;            (make-posn 28 (- HEIGHT TANK-HEIGHT)))
; This is an example of a SIGS based on the second clause of
; the data definition because it contains a UFO, Tank, and
; Missile

;(make-fired (make-posn 20 100)
;            (make-tank 100 3)
;            (make-posn 22 103))
; This is an example of a SIGS based on the second clause of
; the data definition because it contains a UFO, Tank, and
; Missile
