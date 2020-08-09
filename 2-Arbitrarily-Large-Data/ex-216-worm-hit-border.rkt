;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-216-worm-hit-border) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 10)
(define FIELD-SIZE 20)

(define WIDTH (* DIAMETER FIELD-SIZE))
(define HEIGHT (* DIAMETER FIELD-SIZE))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SPART (circle (/ DIAMETER 2) "solid" "red"))

; A Snake-Position is a posn
; interpretation it's position x DIAMATERs right and y DIAMETERs down
(define SN (make-posn 3 4))

; A SW (Snake-World) is structure
(define-struct sw [snake dir])
;  (make-sw Snake-Position Direction)
; interpretation: the position of the snake and the direction it's going
(define SW (make-sw SN "right"))

; A Direction is one of the following strings
; - "left"
; - "right"
; - "up"
; - "down"

; SW -> Image
; Renders the current position of the snake in the world
(check-expect (render (make-sw (make-posn 0 0) "right"))
              (place-image/align SPART 0 0 "left" "top" BACKGROUND))
(check-expect (render (make-sw (make-posn 2 1) "down"))
              (place-image/align
               SPART (* DIAMETER 2) (* DIAMETER 1)
               "left" "top" BACKGROUND))
(define (render sw)
  (place-image/align
   SPART (* DIAMETER (posn-x (sw-snake sw))) (* DIAMETER (posn-y (sw-snake sw)))
   "left" "top" BACKGROUND))

; SW -> Image
; Renders the Game Over scene, saying worm hit border
(check-expect (render-game-over SW)
              (place-image
               (text "worm hit border" (* DIAMETER 1.5) "black")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render SW)))
(define (render-game-over sw)
              (place-image
               (text "worm hit border" (* DIAMETER 1.5) "black")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render
                (make-sw
                 (bring-snake-in (sw-snake sw))
                 (sw-dir sw)))))

; Snake -> Snake
; Keeps a snake inside the word (for final render)
(check-expect (bring-snake-in SN) SN)
(check-expect (bring-snake-in (make-posn -1 0))
              (make-posn 0 0))
(check-expect (bring-snake-in (make-posn FIELD-SIZE 0))
              (make-posn (sub1 FIELD-SIZE) 0))
(check-expect (bring-snake-in (make-posn 0 -1))
              (make-posn 0 0))
(check-expect (bring-snake-in (make-posn 0 FIELD-SIZE))
              (make-posn 0 (sub1 FIELD-SIZE)))
(define (bring-snake-in s)
  (make-posn
   (min (max 0 (posn-x s)) (sub1 FIELD-SIZE))
   (min (max 0 (posn-y s)) (sub1 FIELD-SIZE))))

; SW -> Boolean
; check if the snake died
(check-expect (game-over? SW) #false)
(define (game-over? sw)
  (hit-wall? (sw-snake sw)))

; Snake -> Boolean
; checks to see if the snake hit a wall
(check-expect (hit-wall? (make-posn -1 0)) #true)
(check-expect (hit-wall? (make-posn FIELD-SIZE 0)) #true)
(check-expect (hit-wall? (make-posn 0 -1)) #true)
(check-expect (hit-wall? (make-posn 0 FIELD-SIZE)) #true)
(check-expect (hit-wall? (make-posn 0 0)) #f)
(check-expect (hit-wall? (make-posn (sub1 FIELD-SIZE) 0)) #f)
(check-expect (hit-wall? (make-posn 0 (sub1 FIELD-SIZE))) #f)
(define (hit-wall? s)
  (or
   (>= (posn-x s) FIELD-SIZE)
   (<= (posn-x s) -1)
   (<= (posn-y s) -1)
   (>= (posn-y s) FIELD-SIZE)))

; SW -> SW
; Moves the snake ahead for each tick of the clock
(check-expect (tock (make-sw (make-posn 2 4) "right"))
              (make-sw (make-posn 3 4) "right"))
(check-expect (tock (make-sw (make-posn 2 4) "left"))
              (make-sw (make-posn 1 4) "left"))
(check-expect (tock (make-sw (make-posn 2 4) "up"))
              (make-sw (make-posn 2 3) "up"))
(check-expect (tock (make-sw (make-posn 2 4) "down"))
              (make-sw (make-posn 2 5) "down"))
(define (tock sw)
  (cond
    [(string=? (sw-dir sw) "right") (make-sw
                                     (move "right" (sw-snake sw))
                                     (sw-dir sw))]
    [(string=? (sw-dir sw) "left") (make-sw
                                     (move "left" (sw-snake sw))
                                     (sw-dir sw))]
    [(string=? (sw-dir sw) "up") (make-sw
                                     (move "up" (sw-snake sw))
                                     (sw-dir sw))]
    [(string=? (sw-dir sw) "down") (make-sw
                                     (move "down" (sw-snake sw))
                                     (sw-dir sw))]))

; Snake Direction -> Snake
; Moves the snake one position in d Direction
(check-expect (move "right" SN) (make-posn 4 4))
(check-expect (move "left" SN) (make-posn 2 4))
(check-expect (move "down" SN) (make-posn 3 5))
(check-expect (move "up" SN) (make-posn 3 3))
(define (move d s)
  (cond
    [(string=? d "right") (make-posn (add1 (posn-x s)) (posn-y s))]
    [(string=? d "left") (make-posn (sub1 (posn-x s)) (posn-y s))]
    [(string=? d "up") (make-posn (posn-x s) (sub1 (posn-y s)))]
    [(string=? d "down") (make-posn (posn-x s) (add1 (posn-y s)))]))

; Direction SW -> SW
; changes the direction on the snake in a game to d
(check-expect (change-dir "right" (make-sw (make-posn 2 2) "left"))
              (make-sw (make-posn 2 2) "right"))
(check-expect (change-dir "down" (make-sw (make-posn 2 2) "down"))
              (make-sw (make-posn 2 2) "down"))
(define (change-dir d sw)
  (make-sw (sw-snake sw) d))

; SW KeyEvent -> SW
; Changes the direction of the snake when pressing arrow keys
(check-expect (handle-key (make-sw SN "right") "right")
              (make-sw SN "right"))
(check-expect (handle-key (make-sw SN "right") "left")
              (make-sw SN "right"))
(check-expect (handle-key (make-sw SN "right") "down")
              (make-sw SN "down"))
(check-expect (handle-key (make-sw SN "right") "up")
              (make-sw SN "up"))

(check-expect (handle-key (make-sw SN "left") "left")
              (make-sw SN "left"))
(check-expect (handle-key (make-sw SN "left") "right")
              (make-sw SN "left"))
(check-expect (handle-key (make-sw SN "left") "down")
              (make-sw SN "down"))
(check-expect (handle-key (make-sw SN "left") "up")
              (make-sw SN "up"))

(check-expect (handle-key (make-sw SN "up") "up")
              (make-sw SN "up"))
(check-expect (handle-key (make-sw SN "up") "down")
              (make-sw SN "up"))
(check-expect (handle-key (make-sw SN "up") "right")
              (make-sw SN "right"))
(check-expect (handle-key (make-sw SN "up") "left")
              (make-sw SN "left"))

(check-expect (handle-key (make-sw SN "down") "down")
              (make-sw SN "down"))
(check-expect (handle-key (make-sw SN "down") "up")
              (make-sw SN "down"))
(check-expect (handle-key (make-sw SN "down") "right")
              (make-sw SN "right"))
(check-expect (handle-key (make-sw SN "down") "left")
              (make-sw SN "left"))
(define (handle-key sw ke)
  (if
   (moving-x? (sw-dir sw))
   (cond
    [(key=? ke "up") (change-dir "up" sw)]
    [(key=? ke "down") (change-dir "down" sw)]
    [else sw])
   (cond
    [(key=? ke "right") (change-dir "right" sw)]
    [(key=? ke "left") (change-dir "left" sw)]
    [else sw])))

; Direction -> Boolean?
; Checks if the snake is moving horizontally
(check-expect (moving-x? "right") #true)
(check-expect (moving-x? "left") #true)
(check-expect (moving-x? "up") #false)
(check-expect (moving-x? "down") #false)
(define (moving-x? d)
  (or (string=? d "right")
      (string=? d "left")))

(define (game speed)
  (big-bang SW
    [to-draw render]
    [on-key handle-key]
    [stop-when game-over? render-game-over]
    [on-tick tock speed]))