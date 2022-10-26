;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-527) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define MIN 12)
(define A 12)
(define SCENE (empty-scene 300 300))

; Image Number Number Number
; draws a Savannah tree given:
; base X-coord, base Y-coord, length of base, angle of base 
(define (add-savannah img x y l a)
  (cond
    [(< l MIN) img]
    [else (local
            ((define angle (/ (* a pi) 180))
             (define end-x (+ x (* l (cos angle))))
             (define end-y (- y (* l (sin angle))))
             (define new-img (scene+line img x y end-x end-y "red"))
             (define base (make-posn x y))
             (define end (make-posn end-x end-y))
             (define base1 (eqd-point (/ 1 3) base end))
             (define base2 (eqd-point (/ 2 3) base end))
             (define new-l (* l 0.75)))
            (add-savannah
             (add-savannah new-img
                           (posn-x base1)
                           (posn-y base1)
                           new-l
                           (+ a A))
             (posn-x base2)
             (posn-y base2)
             new-l
             (- a A)))]))


; Number Posn Posn
; Gives the equadistant point on a line n distance along it
(define (eqd-point n a b)
  (make-posn
   (+ (posn-x a) (* n (- (posn-x b) (posn-x a))))
   (+ (posn-y a) (* n (- (posn-y b) (posn-y a))))))



(add-savannah SCENE 150 300 120 90)