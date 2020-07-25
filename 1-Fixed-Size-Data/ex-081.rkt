;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-081) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Hours is a Number
; between 0 (inclusive) and 24 (inclusive)

; A Minutes is a Number
; between 0 (inclusive) and 60 (inclusive)

; A UnboundedMinutes is a Number
; 0 (inclusive) or greater

; A Seconds is a Number
; between 0 (inclusive) and 60 (inclusive)

; A UnboundedSeconds is a Number
; 0 (inclusive) or greater

(define-struct time [hours minutes seconds])
; a Time is a structure
;  (make-timepassed Hours Minutes Seconds)
; interpretation the Hours Minutes and Seconds that
; have passed since midnight

; Time -> UnboundedSeconds
; gives the amount of seconds passed since midnight in a given Time structure
(check-expect (seconds-passed (make-time 0 0 5)) 5)
(check-expect (seconds-passed (make-time 12 30 2)) 45002)
(check-expect (seconds-passed (make-time 0 1 1)) 61)
(define (seconds-passed t)
  (+
   (seconds-in-hours (time-hours t))
   (seconds-in-minutes (time-minutes t))
   (time-seconds t)))

; UnboundedMinutes -> UnboundedSeconds
; gives the total amount of seconds in a given number of Minutes
(check-expect (seconds-in-minutes 0) 0)
(check-expect (seconds-in-minutes 2) 120)
(check-expect (seconds-in-minutes 3) 180)
(define (seconds-in-minutes m)
  (* m 60))

; Hours -> UnboundedSeconds
; gives the total amount of seconds in a given number of Hours
(check-expect (seconds-in-hours 0) 0)
(check-expect (seconds-in-hours 1) 3600)
(define (seconds-in-hours h)
  (seconds-in-minutes (* h 60)))

