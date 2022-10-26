;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-525) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define MIN 20)
(define SCENE (empty-scene 400 400))
(define A (make-posn 200  50))
(define B (make-posn  27 350))
(define C (make-posn 373 350))

; Image Posn Posn Posn -> Image 
; generative adds the triangle (a, b, c) to scene0, 
; subdivides it into three triangles by taking the 
; midpoints of its sides; stop if (a, b, c) is too small
; accumulator the function accumulates the triangles of scene0
(define (add-sierpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        (define mid-b-c (mid-point b c))
        (define mid-c-a (mid-point c a))
        (define scene2
          (add-sierpinski scene1 a mid-a-b mid-c-a))
        (define scene3
          (add-sierpinski scene2 b mid-b-c mid-a-b)))
       ; —IN—
       (add-sierpinski scene3 c mid-c-a mid-b-c))]))

; Image Posn Posn Posn -> Image 
; adds the triangle with enpoints a, b, c to a given image
(define (add-triangle scene a b c)
  (local
    ((define (add-line img start finish)
       (scene+line img
                   (posn-x start)
                   (posn-y start)
                   (posn-x finish)
                   (posn-y finish)
                   "red")))
    (add-line
     (add-line
      (add-line scene a b)
      b c)
     c a)))

; Posn Posn Posn -> Boolean 
; is the triangle a, b, c too small to be divided
(check-expect (too-small? (make-posn 50 0) (make-posn 0 50) (make-posn 50 50))
              #f)
(check-expect (too-small? (make-posn 2 0) (make-posn 0 2) (make-posn 2 2))
              #t)
(define (too-small? a b c)
  (local
    (; Posn Posn -> Number
     ; returns the distance between two positions
     (define (distance a b)
       (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
                (sqr (- (posn-y a) (posn-y b)))))))
    (< (distance a b) MIN)))

; Posn Posn -> Posn 
; determines the midpoint between a and b
(check-expect (mid-point (make-posn 1 1) (make-posn 1 3))
              (make-posn 1 2))
(define (mid-point a b)
  (make-posn
   (* 0.5 (+ (posn-x a) (posn-x b)))
   (* 0.5 (+ (posn-y a) (posn-y b)))))

