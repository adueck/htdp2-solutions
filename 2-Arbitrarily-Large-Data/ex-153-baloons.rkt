;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-153-baloons) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; A List-of-posns is one of:
; - '()
; - (cons posn List-of-posns)

(define SQ (square 10 "outline" "black"))
(define BALLOON (circle 3 "solid" "red"))

; N Image -> Image
; produces a vertical arrangement of n copies of an img
(check-expect (col 3 SQ) (above SQ SQ SQ))
(define (col n img)
  (cond
    [(zero? n) (square 0 "outline" "blue")]
    [else (above img (col (sub1 n) img))]))

; N Image -> Image
; produces a horizontal arrangement of n copies of and img
(check-expect (row 3 SQ) (beside SQ SQ SQ))
(define (row n img)
  (cond
    [(zero? n) (square 0 "outline" "blue")]
    [else (beside img (row (sub1 n) img))]))

(define GRID (col 18 (row 8 SQ)))
(define FRAME (empty-scene (image-width GRID) (image-height GRID)))
(define BACKGROUND (place-image/align GRID 0 0 "left" "top" FRAME))

; List-of-posns Image -> Image
; places a list of balloons based on coordinates
(check-expect (add-balloons '() BACKGROUND) BACKGROUND)
(check-expect (add-balloons
               (cons (make-posn 10 5) '()) BACKGROUND)
              (place-image BALLOON 10 5 BACKGROUND))
(check-expect (add-balloons
               (cons (make-posn 30 30) (cons (make-posn 10 5) '())) BACKGROUND)
              (place-image
               BALLOON 30 30
               (place-image BALLOON 10 5 BACKGROUND)))
(define (add-balloons lop img)
  (cond
    [(empty? lop) img]
    [(cons? lop) (place-image
                  BALLOON
                  (posn-x (first lop))
                  (posn-y (first lop))
                  (add-balloons (rest lop) img))]))
