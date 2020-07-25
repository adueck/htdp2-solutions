;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-073) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))

; Posn -> Image
; adds a red spot to MTS at p
(check-expect (scene+dot (make-posn 10 20))
              (place-image DOT 10 20 MTS))
(check-expect (scene+dot (make-posn 2 4))
              (place-image DOT 2 4 MTS))
(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

; Posn -> Posn
; increases the x-coordinate by 3
(check-expect (x+ (make-posn 2 5)) (make-posn 5 5))
(check-expect (x+ (make-posn 87 1)) (make-posn 90 1))
(define (x+ p)
  (make-posn (+ (posn-x p) 3) (posn-y p)))

; Posn Number -> Posn
; increases the x-coordinate by n
(check-expect (posn-up-x (make-posn 2 5) 2) (make-posn 4 5))
(check-expect (posn-up-x (make-posn 23 6) 7) (make-posn 30 6))
(define (posn-up-x p n)
  (make-posn (+ (posn-x p) n) (posn-y p)))
