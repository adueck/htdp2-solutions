;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-252) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))

; A PosnOrNum is one of:
; - Number
; - Posn

; A NumOrImage is one of:
; - Image
; - Number

; [List-of Number] -> Number
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product
          (rest l)))]))

; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))

(check-expect (fold2 (list 2 4 5) 1 *)
              (product (list 2 4 5)))
(check-expect (fold2 (list (make-posn 2 5)
                           (make-posn 20 8))
                     emt
                     place-dot)
              (image* (list (make-posn 2 5)
                            (make-posn 20 8))))
(define (fold2 l bValue R)
  (cond
    [(empty? l) bValue]
    [else
     (R (first l)
        (fold2 (rest l) bValue R))]))

; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
