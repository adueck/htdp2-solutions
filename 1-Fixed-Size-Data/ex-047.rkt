;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-047) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; maximum Happiness
(define MAX 100)
; minimum Happiness
(define MIN 0)

; guage size - single point of control for physical
; size of guage
(define WIDTH 125)
(define HEIGHT (* WIDTH 1/5))
(define BACKGROUND (empty-scene (+ WIDTH 2) (+ HEIGHT 2)))

; Happiness is a Number between 0 and 100
; interpretation the percentage of the hapiness guage

; Happiness -> Happiness
; decrease happiness by 0.1 ever tick of the clock
; cannot fall below 0 (MIN)
(check-expect (tock 50) 49.9)
(check-expect (tock 0) 0)
(check-expect (tock 100) 99.9)
(define (tock h)
  (cond
    ; guard against strange fractions that might cause
    ; it to dip below 0
    [(> (- h 0.1) 0) (- h 0.1)]
    [else 0]))

; Happiness -> Number
; gives the number of pixels in width to draw the Happiness guage
; based on the value of the Happiness and the physical size of the guage
(check-expect (guage-x 50) (* WIDTH (/ 50 MAX)))
(check-expect (guage-x 0) 0)
(check-expect (guage-x 100) (* WIDTH (/ 100 MAX)))
(define (guage-x h)
  (cond
    [(= h 0) 0]
    [else (* WIDTH (/ h MAX))]))

; Happiness -> Image
; renders the hapiness gauge shown by a red bar
(check-expect (render 50) (place-image/align (rectangle (guage-x 50) HEIGHT "solid" "red") 1 1 "left" "top" BACKGROUND))
(check-expect (render 100) (place-image/align (rectangle (guage-x 100) HEIGHT "solid" "red") 1 1 "left" "top" BACKGROUND))
(check-expect (render 0) (place-image/align (rectangle (guage-x 0) HEIGHT "solid" "red") 1 1 "left" "top" BACKGROUND))
(define (render h)
  (place-image/align
   (rectangle (guage-x h) HEIGHT "solid" "red")
   1 1
   "left" "top"
   BACKGROUND))

; Happiness String -> Happiness
; increases happiness by 1/5 on "down" key
; increases happiness by 1/3 on "up" key
; should not go over MAX
(check-expect (handle-key 25 "down") 30)
(check-expect (handle-key 30 "up") 40)
(check-expect (handle-key 50 "left") 50)
(check-expect (handle-key (- MAX 1) "up") 100)
(define (handle-key h ke)
  (cond
    [(string=? ke "down") (up-by-n h 5)]
    [(string=? ke "up") (up-by-n h 3)]
    [else h]))

; Happiness Number -> Hapiness
; increase Happiness by 1/n but do not allow
; to go over MAX
(check-expect (up-by-n 30 3) 40)
(check-expect (up-by-n 50 5) 60)
(check-expect (up-by-n (- MAX 1) 3) MAX)
(define (up-by-n h n)
  (min MAX (+ h (/ h n))))

(define (guage-prog h)
  (big-bang h
    [on-tick tock]
    [on-key handle-key]
    [to-draw render]))
