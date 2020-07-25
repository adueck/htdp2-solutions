;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-051) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A TrafficLight is one of the followings Strings:
; - "red"
; - "green"
; - "yellow"
; interpretation the three strings represent the three
; possible states that the traffic light may assume

; the radius of the traffic light, in pixels
(define LIGHT-RADIUS 25)

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(check-expect (traffic-light-next "green") "yellow")
(check-expect (traffic-light-next "yellow") "red")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; TrafficLight -> Image
; yields an image of a traffic light with the current
; color given current state s
(check-expect (render "red") (circle LIGHT-RADIUS "solid" "red"))
(check-expect (render "green") (circle LIGHT-RADIUS "solid" "green"))
(check-expect (render "yellow") (circle LIGHT-RADIUS "solid" "yellow"))
(define (render s) (circle LIGHT-RADIUS "solid" s))

; WorldState -> WorldState
(define (traffic-light s)
  (big-bang s
    [on-tick traffic-light-next]
    [to-draw render]))
