;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-528) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define MIN 20)
(define A 12)
(define SCENE (empty-scene 300 300))

(define-struct bd [a b c bezier])
; A BezierData is a Structure
;  (make-bd [Posn Posn Posn Bezier])
; Gives the original points used to create a bezier
; along with the information (small lines) to draw it

; A Bezier is a [List-of [Posn Posn]]
; interpretation pairs of posn for all the straight lines
; it takes to draw the "curve"

; Posn Posn Posn -> BezierData
(define (make-bezier-data a b c)
  (local
    ((define bezier (make-bezier a b c)))
    (make-bd a b c bezier)))

; Posn Posn Posn -> Bezier
; Draws a bezier curve with points a b c on SCENE
(define (make-bezier a b c)
  (cond
    [(< (distance a c) MIN)
     (list (list (make-posn (posn-x a) (posn-y a))
                 (make-posn (posn-x c) (posn-y c))))]
    [else (local
            ((define a-b (mid-point a b))
             (define b-c (mid-point b c))
             (define a-b-c (mid-point a-b b-c)))
           
             (append
              (make-bezier a a-b a-b-c) 
              (make-bezier a-b-c b-c c)))]))

; Bezier -> Image
(define (draw-bezier bz)
  (cond
    [(empty? bz) SCENE]
    [else (local
            ((define line (first bz))
             (define a (first line))
             (define b (second line)))
            (scene+line
             (draw-bezier (rest bz))
             (posn-x a) (posn-y a)
             (posn-x b) (posn-y b)
             "red"))]))
            
; BezierData -> Image
(define (draw-bezier-data bd)
  (local
    ((define (draw-bl img a b)
       (scene+line img
                   (posn-x a) (posn-y a)
                   (posn-x b) (posn-y b)
                   "blue")))
  (draw-bl
   (draw-bl (draw-bezier (bd-bezier bd))
            (bd-a bd) (bd-b bd))
   (bd-b bd) (bd-c bd))))

; Posn Posn -> Posn 
; determines the midpoint between a and b
(define (mid-point a b)
  (make-posn
   (* 0.5 (+ (posn-x a) (posn-x b)))
   (* 0.5 (+ (posn-y a) (posn-y b)))))

; Posn Posn -> Number
; returns the distance between two positions
(define (distance a b)
  (sqrt (+ (sqr (- (posn-x a) (posn-x b)))
           (sqr (- (posn-y a) (posn-y b))))))

; Posn Posn Posn Boolean -> Image
(define (generate-bezier a b c incD)
  (local
    ((define bd (make-bezier-data a b c)))
    (if incD
        (draw-bezier-data bd)
        (draw-bezier (bd-bezier bd)))))

(generate-bezier (make-posn 20 10)
                 (make-posn 100 280)
                 (make-posn 200 150)
                 #f)