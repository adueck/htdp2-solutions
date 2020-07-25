;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-072) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; an AreaCode is an interval
; between 100 - 999

; a PhoneNumber is an interval
; between 1000 - 9999

; a Switch is an interval
; between 100 - 999

(define-struct phone [area num])
; a Phone is a structure:
;  (make-entry Number Number)
; interpretation an AreaCode and PhoneNumber for a phone number

(define-struct phone# [area switch num])
; a Phone# is a structure:
;  (make-entry Number Number Number)
; interpretation an AreaCode, Switch, and PhoneNumber for a phone number