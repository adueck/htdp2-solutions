;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-218-worm-with-tails-and-game-end) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 10)
(define FIELD-SIZE 20)

(define WIDTH (* DIAMETER FIELD-SIZE))
(define HEIGHT (* DIAMETER FIELD-SIZE))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SPART (circle (/ DIAMETER 2) "solid" "red"))

; A Snake-Position is a NEList-of-posns
; interpretation each member of the list is
; the position of one item of the snake's body
; x DIAMATERs right and y DIAMETERs down
(define SN (list (make-posn 3 4)))
(define SN2 (list (make-posn 4 4) (make-posn 3 4)))
(define SN3 (list (make-posn 2 6)
                  (make-posn 2 5)
                  (make-posn 2 4)
                  (make-posn 2 3)
                  (make-posn 2 2)))

; A SW (Snake-World) is structure
(define-struct sw [snake dir])
;  (make-sw Snake-Position Direction)
; interpretation: the position of the snake and the direction it's going
(define SW (make-sw SN2 "right"))

; A Direction is one of the following strings
; - "left"
; - "right"
; - "up"
; - "down"

; SW -> Image
; Renders the current position of the snake in the world
(check-expect (render SW) (draw-snake (sw-snake SW)))
(define (render sw)
  (draw-snake (sw-snake sw)))

; SW -> Image
; Renders the game over screen
(define (render-game-over sw)
  (place-image
   (text
    (if (hit-wall? (sw-snake sw))
        "snake hit the wall"
        "snake hit itself")
         (* DIAMETER 1.5) "black")
   (/ HEIGHT 2) (/ WIDTH 2)
   (render sw)))

; Snake -> Image
; Draws a snake
(check-expect (draw-snake SN)
              (place-image/align
               SPART (* DIAMETER 3) (* DIAMETER 4)
               "left" "top" BACKGROUND))
(check-expect (draw-snake SN2)
              (place-image/align
               SPART (* DIAMETER 4) (* DIAMETER 4)
               "left" "top"
               (place-image/align
                SPART (* DIAMETER 3) (* DIAMETER 4)
                "left" "top" BACKGROUND)))
(define (draw-snake s)
  (cond
    [(empty? s) BACKGROUND]
    [else (place-image/align
           SPART
           (* DIAMETER (posn-x (first s)))
           (* DIAMETER (posn-y (first s)))
           "left" "top"
           (draw-snake (rest s)))]))

; SW -> SW
; Moves the snake ahead for each tick of the clock
(check-expect (tock (make-sw SN "right"))
              (make-sw (list (make-posn 4 4)) "right"))
(check-expect (tock (make-sw SN "left"))
              (make-sw (list (make-posn 2 4)) "left"))
(check-expect (tock (make-sw SN "down"))
              (make-sw (list (make-posn 3 5)) "down"))
(check-expect (tock (make-sw SN "up"))
              (make-sw (list (make-posn 3 3)) "up"))
(define (tock sw)
  (cond
    [(string=? (sw-dir sw) "right") (make-sw
                                     (move-snake 1 0 (sw-snake sw))
                                     (sw-dir sw))]
    [(string=? (sw-dir sw) "left") (make-sw
                                     (move-snake -1 0 (sw-snake sw))
                                     (sw-dir sw))]
    [(string=? (sw-dir sw) "up") (make-sw
                                     (move-snake 0 -1 (sw-snake sw))
                                     (sw-dir sw))]
    [(string=? (sw-dir sw) "down") (make-sw
                                     (move-snake 0 1 (sw-snake sw))
                                     (sw-dir sw))]))

; Number Number Snake -> Snake
; Moves the head of the snake by x y
(check-expect (move-snake 1 0 SN) (list (make-posn 4 4)))
(check-expect (move-snake 0 -1 SN2) (list
                                     (make-posn 4 3)
                                     (make-posn 4 4)))
(define (move-snake x y s)
  (cons
   (make-new-head x y (first s))
   (delete-last s)))

; Number Number Posn -> Posn
; makes the positiong for a new head x y over from old p
(check-expect (make-new-head 1 0 (make-posn 3 4))
              (make-posn 4 4))
(check-expect (make-new-head 0 1 (make-posn 3 4))
              (make-posn 3 5))
(define (make-new-head x y p)
  (make-posn (+ (posn-x p) x)
             (+ (posn-y p) y)))

; Snake -> Snake
; deletes the last position from a snake
(check-expect (delete-last (list (make-posn 2 3)
                                 (make-posn 2 2)))
                           (list (make-posn 2 3)))
(check-expect (delete-last (list (make-posn 3 4))) '())
(define (delete-last s)
  (cond
    [(empty? (rest s)) '()]
    [else (cons (first s) (delete-last (rest s)))]))

; Direction SW -> SW
; changes the direction on the snake in a game to d
(check-expect (change-dir "right" (make-sw SN2 "left"))
              (make-sw SN2 "right"))
(check-expect (change-dir "down" (make-sw SN2 "down"))
              (make-sw SN2 "down"))
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

; SW -> Boolean
; Checks if the snake ran into a wall or itself
(define (collision? sw)
  (or
   (hit-wall? (sw-snake sw))
   (hit-itself? (sw-snake sw))))

; Snake -> Boolean
; Checks if the snake will have run past a wall
(check-expect (hit-wall? (list (make-posn -1 2))) #true)
(check-expect (hit-wall? (list (make-posn 2 -1))) #true)
(check-expect (hit-wall? (list (make-posn 1 1))) #false)
(check-expect (hit-wall? (list (make-posn FIELD-SIZE 1))) #true)
(check-expect (hit-wall? (list (make-posn 1 FIELD-SIZE))) #true)
(define (hit-wall? s)
  (or
   (< (posn-x (first s)) 0)
   (>= (posn-x (first s)) FIELD-SIZE)
   (< (posn-y (first s)) 0)
   (>= (posn-y (first s)) FIELD-SIZE)))

; Snake -> Boolean
(define (hit-itself? s)
  (member? (first s) (rest s)))

; Direction -> Boolean
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
    [stop-when collision? render-game-over]
    [on-key handle-key]
    [on-tick tock speed]))