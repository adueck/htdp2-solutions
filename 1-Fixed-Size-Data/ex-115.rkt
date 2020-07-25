;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-115) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

(define MESSAGE
  "traffic light expected, given some other value")

; Any Any -> Boolean
; are the two values elements of TrafficLight and, 
; if so, are they equal
(check-expect (light=? "red" "red") #true)
(check-expect (light=? "red" "green") #false)
(check-expect (light=? "green" "green") #true)
(check-expect (light=? "yellow" "yellow") #true)
(check-error (light=? 3 "yellow") "the first value is not a traffic light")
(check-error (light=? "green" "apple") "the second value is not a traffic light")
(define (light=? a-value another-value)
  (cond
    [(not (light? a-value)) (error "the first value is not a traffic light")]
    [(not (light? another-value)) (error "the second value is not a traffic light")]
    [else (string=? a-value another-value)]))

; Any -> Boolean
; is the given value an element of TrafficLight
(check-expect (light? "red") #true)
(check-expect (light? "c") #false)
(check-expect (light? 1) #false)
(define (light? x)
  (cond
    [(string? x) (or (string=? "red" x)
                     (string=? "green" x)
                     (string=? "yellow" x))]
    [else #false]))