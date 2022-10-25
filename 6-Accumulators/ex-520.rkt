;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-520) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define fig-initial
  (list (make-ps (make-people 3 3)
                 (make-people 0 0)
                 "left")))
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