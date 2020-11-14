;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-272) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; [List-of X] [List-of X] -> [List-of X]
; appends to lists
(check-expect (append-from-fold
               (list 1 2 3) (list 4 5 6))
              (list 1 2 3 4 5 6))
(check-expect (append-from-fold
               (list "a" "b") (list "c"))
              (list "a" "b" "c"))
(define (append-from-fold li1 li2)
  (foldr cons li2 li1))

; [List-of Number] -> Number
; computes the sum of a list of numbers
(check-expect (sum-all '(2 1 3)) 6)
(check-expect (sum-all '(5 2)) 7)
(define (sum-all lon)
  (foldr + 0 lon))

; [List-of Number] -> Number
; computes the product of a list of numbers
(check-expect (prod-all '(2 1 3)) 6)
(check-expect (prod-all '(5 2)) 10)
(define (prod-all lon)
  (foldr * 1 lon))

; [List-of Image] -> Image
; horizontally composes a list of Images
(define im1 (triangle 40 "solid" "tan"))
(define im2 (right-triangle 36 48 "solid" "black"))
(define im3 (star 40 "solid" "gray"))
(check-expect (compose-horz-imgs (list im1 im2 im3))
              (beside im1 im2 im3))
(define (compose-horz-imgs loi)
  (foldr beside empty-image loi))

; can't use foldl because then the images are reversed

; [List-of Image] -> Image
; vertically composes a list of Images
(check-expect (compose-vert-imgs (list im1 im2 im3))
              (above im1 im2 im3))
(define (compose-vert-imgs loi)
  (foldr above empty-image loi))

