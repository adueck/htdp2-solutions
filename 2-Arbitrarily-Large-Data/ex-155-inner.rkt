;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-155-inner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])
; An RD (short for Russian doll) is one of: 
; – String 
; – (make-layer String RD)

; RD -> Number
; how many dolls are part of an-rd
(check-expect (depth "red") 1)
(check-expect
  (depth
   (make-layer "yellow" (make-layer "green" "red")))
  3)
(define (depth an-rd)
  (cond
    [(string? an-rd) 1]
    [(layer? an-rd) (+ (depth (layer-doll an-rd)) 1)]))

; RD -> String
; Produces a list of the colors of a list of Russian dolls
(check-expect (colors "red") "red")
(check-expect (colors (make-layer "blue" "red"))
              "blue, red")
(check-expect (colors
               (make-layer "blue" (make-layer "white" "red")))
              "blue, white, red")
(define (colors an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd)
     (string-append
      (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))

; RD -> String
; Produces the color of the innermost doll in a list of
; Russian dolls
(check-expect (inner "red") "red")
(check-expect (inner (make-layer "blue" "red"))
              "red")
(check-expect (inner
               (make-layer "blue" (make-layer "white" "yellow")))
              "yellow")
(define (inner an-rd)
  (cond
    [(string? an-rd) an-rd]
    [(layer? an-rd) (inner (layer-doll an-rd))]))
