;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-281) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)

; 1. consumes a number and decides whether it is less than 10;
(lambda (x) (< x 10))

; 2. multiplies two given numbers and turns the result into a string;\
(lambda (x y) (number->string (* x y)))

; 3. consumes a natural number and returns 0 for evens and 1 for odds;
(lambda (x) (modulo x 2))

; 4. consumes two inventory records and compares them by price;
(lambda (x y)
  (>= (ir-price x) (ir-price y)))

; 5. adds a red dot at a given Posn to a given Image.
(lambda (i p)
  (local ((define red-dot (circle 2 "solid" "red")))
  (overlay/xy red-dot (- (posn-x p)) (- (posn-y p)) i)))