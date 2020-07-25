;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-159-riot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; An N is one of: 
; – 0
; – (add1 N)
; interpretation represents the counting numbers

; A List-of-posns is one of:
; - '()
; - (cons posn List-of-posns)

(define-struct pair [balloon# lob])
; A Pair is a structure (make-pair N List-of-posns)
; A List-of-posns is one of: 
; – '()
; – (cons Posn List-of-posns)
; interpretation (make-pair n lob) means n balloons 
; must yet be thrown and added to lob

; A BaloonWorld is a Pair
; interpretation the amount of baloons to be thrown
; and the status of the baloons thrown

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
(define MAX-X (image-width BACKGROUND))
(define MAX-Y (image-height BACKGROUND)) 

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

; List-of-posns -> List-of-posns
; adds a balloon the the list of baloons
; with random coordinates
(check-random (throw-balloon '())
              (cons
               (make-posn (random MAX-X) (random MAX-Y))
               '()))
(check-random (throw-balloon (cons (make-posn 20 20) '()))
              (cons
               (make-posn (random MAX-X) (random MAX-Y))
                (cons (make-posn 20 20) '())))
(define (throw-balloon lob)
  (cons
   (make-posn (random MAX-X) (random MAX-Y))
   lob))

; BalloonWorld -> BalloonWorld
; throws a new balloon every second, until all are thrown
(check-random (tock
               (make-pair 3 '()))
              (make-pair
                2 (cons
                   (make-posn (random MAX-X) (random MAX-Y))
                   '())))
(check-expect (tock
               (make-pair 0 '()))
              (make-pair 0 '()))
(define (tock w)
  (cond
    [(zero? (pair-balloon# w)) w]
    [else (make-pair
           (sub1 (pair-balloon# w))
           (throw-balloon (pair-lob w)))]))

; BaloonWold -> Image
; Renders the state of the baloon world
(check-expect (render (make-pair 2 '()))
              BACKGROUND)
(check-expect (render (make-pair
                       3 (cons (make-posn 20 10) '())))
              (add-balloons
               (cons (make-posn 20 10) '())
               BACKGROUND))
(define (render w)
  (add-balloons (pair-lob w) BACKGROUND))

(define (riot n)
  (big-bang (make-pair n '())
    [to-draw render]
    [on-tick tock 1]))
