;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-060) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define LIGHT-RAD 10)
(define HEIGHT (* LIGHT-RAD 3.3))
(define WIDTH (* HEIGHT 2.7))
(define CENTER-X (/ WIDTH 2))
(define LIGHT-BOX (empty-scene WIDTH HEIGHT))
(define LIGHT-POSNS
  (list
   (make-posn (- CENTER-X (* LIGHT-RAD 2.6)) (/ HEIGHT 2))
   (make-posn CENTER-X (/ HEIGHT 2))
   (make-posn (+ CENTER-X (* LIGHT-RAD 2.6)) (/ HEIGHT 2))))
 
; An N-TrafficLight is one of:
; – 0 interpretation the traffic light shows red
; – 1 interpretation the traffic light shows green
; – 2 interpretation the traffic light shows yellow
	
; N-TrafficLight -> N-TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)
(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

; N-TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render 1) (place-images
 (list (circle LIGHT-RAD "solid" "green")
       (circle LIGHT-RAD "outline" "yellow")
       (circle LIGHT-RAD "outline" "red"))
 LIGHT-POSNS
 LIGHT-BOX))
(check-expect (tl-render 2) (place-images
 (list (circle LIGHT-RAD "outline" "green")
       (circle LIGHT-RAD "solid" "yellow")
       (circle LIGHT-RAD "outline" "red"))
 LIGHT-POSNS
 LIGHT-BOX))
(check-expect (tl-render 0) (place-images
 (list (circle LIGHT-RAD "outline" "green")
       (circle LIGHT-RAD "outline" "yellow")
       (circle LIGHT-RAD "solid" "red"))
 LIGHT-POSNS
 LIGHT-BOX))

(define (tl-render cs)
  (place-images
    (list
     (circle
       LIGHT-RAD
       (if (= cs 1) "solid" "outline")
       "green")
     (circle
       LIGHT-RAD
       (if (= cs 2) "solid" "outline")
       "yellow")
     (circle
       LIGHT-RAD
       (if (= cs 0) "solid" "outline")
       "red"))
    LIGHT-POSNS
    LIGHT-BOX))

; N-TrafficLight -> N-TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next-numeric 1]))

; the tl-next function might declare it's intention a little more
; clearly because we can see the color changes right in the function
