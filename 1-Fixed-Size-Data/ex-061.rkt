;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-061) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define RED "red")
(define GREEN "yellow")
(define YELLOW "green")
 
; An S-TrafficLight is one of:
; – RED
; – GREEN
; – YELLOW

; S-TrafficLight -> S-TrafficLight
; yields the next state, given current state cs
(check-expect (tl-next-symbolic RED) GREEN)
(check-expect (tl-next-symbolic GREEN) YELLOW)
(check-expect (tl-next-symbolic YELLOW) RED)	
(define (tl-next-symbolic cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))


; S-TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render GREEN) (place-images
 (list (circle LIGHT-RAD "solid" "green")
       (circle LIGHT-RAD "outline" "yellow")
       (circle LIGHT-RAD "outline" "red"))
 LIGHT-POSNS
 LIGHT-BOX))
(check-expect (tl-render YELLOW) (place-images
 (list (circle LIGHT-RAD "outline" "green")
       (circle LIGHT-RAD "solid" "yellow")
       (circle LIGHT-RAD "outline" "red"))
 LIGHT-POSNS
 LIGHT-BOX))
(check-expect (tl-render RED) (place-images
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
       (if (equal? cs GREEN) "solid" "outline")
       "green")
     (circle
       LIGHT-RAD
       (if (equal? cs YELLOW) "solid" "outline")
       "yellow")
     (circle
       LIGHT-RAD
       (if (equal? cs RED) "solid" "outline")
       "red"))
    LIGHT-POSNS
    LIGHT-BOX))

; S-TrafficLight -> S-TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next-symbolic 1]))

; the tl-next-symbolic function continues to work even when
; the constants used in S-TrafficLight are changed
