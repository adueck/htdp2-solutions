;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-521) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)

(define-struct ps [left right boat])
; a PuzzleState is a structure
;  (make-ps [People People BoatPosn])
; interpretation the People on both sides of the river
; as well as the position of the boat

(define-struct people [miss cann])
; a People in a structure
;  (make-people [number number])
; interpretation the amount of Miss' and Cann's in a given group of people

; a BoatPosn is one of:
; - "left"
; - "right"
; interpretation the side of the river that the boat is on

(define initial (make-ps (make-people 3 3)
                         (make-people 0 0)
                         "left"))
(define fig-initial (list initial))
(define fig-intermediate
  (list (make-ps (make-people 3 2)
                 (make-people 0 1)
                 "right")
        (make-ps (make-people 3 1)
                 (make-people 0 2)
                 "right")
        (make-ps (make-people 2 2)
                 (make-people 1 1)
                 "right")
        (make-ps (make-people 1 3)
                 (make-people 2 0)
                 "right")
        (make-ps (make-people 2 3)
                 (make-people 1 0)
                 "right")))
(define fig-final
  (list (make-ps (make-people 2 3)
                 (make-people 1 0)
                 "left")
        (make-ps (make-people 3 3)
                 (make-people 0 0)
                 "left")
        (make-ps (make-people 3 2)
                 (make-people 0 1)
                 "left")))

; PuzzleState -> Boolean
; Determines if all the people are on the right river bank
(check-expect (final? (first fig-initial)) #f)
(check-expect (final? (make-ps (make-people 0 0)
                               (make-people 3 3)
                               "right")) #t)
(define (final? ps)
  (local ((define rs (ps-right ps)))
    (and (= 3 (people-miss rs))
         (= 3 (people-cann rs)))))

; PuzzleState -> Image
; Renders a puzzle state
(define (render-ps ps)
  (local
    ((define pRad 10)
     (define MISS (circle pRad "outline" "black"))
     (define CANN (circle pRad "solid" "black"))
     (define BOAT (rectangle (* pRad 2) pRad "solid" "brown"))
     ; People Image -> Image
     ; Renders a set of people
     (define (render-people p)
       (local
         ((define (render-line img n)
            (cond
              [(= 0 n) empty-image]
              [else (above img (render-line img (sub1 n)))])))
         (beside
          (render-line CANN (people-cann p))
          (render-line MISS (people-miss p)))))
     (define height (* 8 pRad))
     (define BANK (rectangle (* 6 pRad) height "outline" "black"))
     (define WATER (rectangle (* 10 pRad) height "solid" "blue"))
     (define BG (beside BANK WATER BANK)))
  (overlay/align/offset
   "left" "top"
   BOAT
   (if (string=? (ps-boat ps) "left")
       (- (image-width BANK))
       (- (- (image-width BG) (+ (image-width BOAT) (image-width BANK)))))
   (- (- (/ height 2) (/ pRad 2)))
   (overlay/align/offset
    "right" "top"
    (render-people (ps-right ps))
    (+ (/ pRad 2)) (- (/ pRad 2))
    (overlay/align/offset
     "left" "top"
     (render-people (ps-left ps))
     (- (/ pRad 2)) (- (/ pRad 2))
     BG)))))
