;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-036) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Image -> Number
; counts the number of pixels in an image
; given: (square 10 "solid" "red"), expect: 100
; given: (rectangle 10 5 "solid" "blue"), expect: 50
(define (image-area img)
  (* (image-height img) (image-width img)))



