;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-104) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vehicle [type passengers plate consumption])
; A Vehicle is a structure:
;  (make-vehicle VehicleType Number String Consumption)

; A Consumption is a Number
; interpretation n miles per gallon in fuel consumption

; A VehicleType is one of the following strings:
; - "car"
; - "van"
; - "bus"
; - "SUV"

(define (vehicle-tepmlate v)
  (... (vehicle-type v) ...
   ... (vehicle-passengers v) ...
   ... (vehicle-plate v) ...
   ... (vehicle-consumption v) ...))

