;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-059) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

; TrafficLight -> TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(check-expect (tl-next "red") "green")
(define (tl-next cs)
  (cond
    [(string=? cs "green") "yellow"]
    [(string=? cs "yellow") "red"]
    [(string=? cs "red") "green"]))
 
; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render "green") (place-images
 (list (circle LIGHT-RAD "solid" "green")
       (circle LIGHT-RAD "outline" "yellow")
       (circle LIGHT-RAD "outline" "red"))
 LIGHT-POSNS
 LIGHT-BOX))
(check-expect (tl-render "yellow") (place-images
 (list (circle LIGHT-RAD "outline" "green")
       (circle LIGHT-RAD "solid" "yellow")
       (circle LIGHT-RAD "outline" "red"))
 LIGHT-POSNS
 LIGHT-BOX))
(check-expect (tl-render "red") (place-images
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
       (if (string=? cs "green") "solid" "outline")
       "green")
     (circle
       LIGHT-RAD
       (if (string=? cs "yellow") "solid" "outline")
       "yellow")
     (circle
       LIGHT-RAD
       (if (string=? cs "red") "solid" "outline")
       "red"))
    LIGHT-POSNS
    LIGHT-BOX))

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))
