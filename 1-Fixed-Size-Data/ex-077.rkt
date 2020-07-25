;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-077) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An Hours is a Number
; between 0 (inclusive) and 24 (inclusive)

; A Minutes is a Number
; between 0 (inclusive) and 60 (inclusive)

; A Seconds is a Number
; between 0 (inclusive) and 60 (inclusive)

(define-struct timepassed [hours minutes seconds])
; a Timepassed is a structure
;  (make-timepassed Hours Minutes Seconds)
; interpretation the Hours Minutes and Seconds that
; have passed since midnight
