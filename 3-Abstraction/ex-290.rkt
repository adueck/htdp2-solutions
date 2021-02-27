;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-290) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(check-expect (append-from-fold (list 1 2) (list 3 4)) (list 1 2 3 4))
(check-expect (append-from-fold '() '()) '())
; [List-of Number] [List-of Number] -> [List-of Number]
(define (append-from-fold l1 l2)
  (foldr
   (lambda (cur existing) (cons cur existing))
   l2
   l1))

(check-expect (sum-from-fold (list 2 3 1)) 6)
; [List-of Number] -> Number
(define (sum-from-fold l)
  (foldl
   (lambda (cur acc) (+ cur acc))
   0
   l))

(check-expect (product-from-fold (list 2 3 2 1)) 12)
; [List-of Number] -> Number
(define (product-from-fold l)
  (foldl
   (lambda (cur acc) (* cur acc))
   1
   l))

(check-expect (hz-imgs '()) empty-image)
(check-expect (hz-imgs (list (circle 3 "solid" "red") (square 5 "solid" "blue")))
              (beside (circle 3 "solid" "red") (square 5 "solid" "blue")))
; [List-of Image] -> Image
(define (hz-imgs loi)
  (foldr
   (lambda (cur acc) (beside cur acc))
   empty-image
   loi))

(check-expect (vert-imgs '()) empty-image)
(check-expect (vert-imgs (list (circle 3 "solid" "red") (square 5 "solid" "blue")))
              (above (circle 3 "solid" "red") (square 5 "solid" "blue")))
; [List-of Image] -> Image
(define (vert-imgs loi)
  (foldr
   (lambda (cur acc) (above cur acc))
   empty-image
   loi))

; A Direction is on of the following strings
; - "horizontal"
; - "vertical"

(check-expect (line-up-imgs '() "vertical") empty-image)
(check-expect (line-up-imgs
               (list (circle 3 "solid" "red") (square 5 "solid" "blue"))
               "vertical")
              (above (circle 3 "solid" "red") (square 5 "solid" "blue")))
(check-expect (line-up-imgs '() "horizontal") empty-image)
(check-expect (line-up-imgs
               (list (circle 3 "solid" "red") (square 5 "solid" "blue"))
               "horizontal")
              (beside (circle 3 "solid" "red") (square 5 "solid" "blue")))
; [List-of Image] Direction -> Image
(define (line-up-imgs loi dir)
    (foldr
     (lambda (cur acc)
       ((if (string=? dir "horizontal") beside above) cur acc))
     empty-image
     loi))