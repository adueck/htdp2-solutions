;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-218-worm-with-food) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define DIAMETER 15)
(define FIELD-SIZE 20)

(define WIDTH (* DIAMETER FIELD-SIZE))
(define HEIGHT (* DIAMETER FIELD-SIZE))
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SPART (circle (/ DIAMETER 2) "solid" "red"))
(define FOOD (circle (/ DIAMETER 2) "solid" "green"))

; A Snake is a NEList-of-posns
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

; A Food is a Posn
; the position of the food
; x DIAMATERs right and y DIAMETERs down
(define FD (make-posn 10 11))

; A SW (Snake-World) is structure
(define-struct sw [snake dir food])
;  (make-sw Snake Direction Food)
; interpretation: the position of the snake and the direction it's going
; the location of the piece of food
(define SW (make-sw SN2 "right" (make-posn 10 11)))

; A Direction is one of the following strings
; - "left"
; - "right"
; - "up"
; - "down"

; SW -> Image
; Renders the current position of the snake in the world
(check-expect (render SW) (draw-score (sw-snake SW)
                                      (draw-food (sw-food SW)
                                                 (draw-snake (sw-snake SW)))))
(define (render sw)
 (draw-score (sw-snake sw)
  (draw-food (sw-food sw)
   (draw-snake (sw-snake sw)))))

; SW -> Image
; Renders the game over screen
(define SHI (make-sw (list (make-posn 0 0)
                           (make-posn 0 1)
                           (make-posn 0 2)
                           (make-posn 1 2)
                           (make-posn 2 2)
                           (make-posn 1 2)
                           (make-posn 0 2)) "up" (make-posn 3 4)))
(check-expect (render-game-over
               (make-sw (list (make-posn -1 0)) "left" (make-posn 5 4)))
              (place-image
               (text "snake hit the wall" (* DIAMETER 2) "black")
               (/ HEIGHT 2) (/ WIDTH 2)
               (render
                (make-sw (list (make-posn -1 0)) "left" (make-posn 5 4)))))
(check-expect (render-game-over SHI)
              (place-image
               (text "snake hit itself" (* DIAMETER 2) "black")
               (/ HEIGHT 2) (/ WIDTH 2)
               (render SHI)))
(define (render-game-over sw)
  (place-image
   (text
    (if (hit-wall? (sw-snake sw))
        "snake hit the wall"
        "snake hit itself")
        (* DIAMETER 2) "black")
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

; Food Image -> Image
; Draws the food over a game scene
(check-expect (draw-food (make-posn 3 4) BACKGROUND)
              (place-image/align
               FOOD (* DIAMETER 3) (* DIAMETER 4)
               "left" "top" BACKGROUND))
(define (draw-food p im)
  (place-image/align
     FOOD (* DIAMETER (posn-x p)) (* DIAMETER (posn-y p))
     "left" "top" im))

; Snake Image -> Image
; Draws the score (based on the snake length) over a game scene
(check-expect (draw-score (list (make-posn 1 1)) BACKGROUND)
              (place-image
               (text "1" DIAMETER "black")
               DIAMETER DIAMETER
               BACKGROUND))
(define (draw-score s im)
  (place-image
   (text
    (number->string (length s))
    DIAMETER "black")
   DIAMETER DIAMETER
   im))

; SW -> SW
; Handles the action for the snake on each tick of the clock
(define (tock sw)
  (if (will-eat? sw)
      (eat sw)
      (handle-move sw)))

; SW -> SW
; determines if the snake will eat the food on the next turn
(check-expect (will-eat? (make-sw (list (make-posn 1 1)) "right" (make-posn 2 1)))
              #t)
(check-expect (will-eat? (make-sw (list (make-posn 1 1)) "left" (make-posn 2 1)))
              #f)
(check-expect (will-eat? (make-sw (list (make-posn 1 1)) "left" (make-posn 0 1)))
              #t)
(check-expect (will-eat? (make-sw (list (make-posn 1 1)) "down" (make-posn 1 2)))
              #t)
(check-expect (will-eat? (make-sw (list (make-posn 1 1)) "down" (make-posn 1 3)))
              #f)
(check-expect (will-eat? (make-sw (list (make-posn 1 1)) "up" (make-posn 1 0)))
              #t)
(define (will-eat? sw)
  (will-hit? (first (sw-snake sw)) (sw-dir sw) (sw-food sw)))

; Posn Direction Posn -> Boolean?
; Determines if a heading in direction d will hit b
(define (will-hit? a dir b)
  (cond
    [(string=? dir "right") (and
                             (= (posn-x a) (sub1 (posn-x b)))
                             (= (posn-y a) (posn-y b)))]
    [(string=? dir "left")  (and
                             (= (sub1 (posn-x a)) (posn-x b))
                             (= (posn-y a) (posn-y b)))]
    [(string=? dir "up") (and
                             (= (posn-x a) (posn-x b))
                             (= (sub1 (posn-y a)) (posn-y b)))]
    [(string=? dir "down") (and
                             (= (posn-x a) (posn-x b))
                             (= (posn-y a) (sub1 (posn-y b))))]))

; SW -> SW
; eats the piece of food and lengthens the snake, replacing the food
; with a new one
(define (eat sw)
  (make-sw
   (cons (sw-food sw) (sw-snake sw))
   (sw-dir sw)
   (food-create (sw-snake sw))))

; SW -> SW
; Moves the snake ahead for each tick of the clock
(check-expect (handle-move (make-sw SN "right" FD))
              (make-sw (list (make-posn 4 4)) "right" FD))
(check-expect (handle-move (make-sw SN "left" FD))
              (make-sw (list (make-posn 2 4)) "left" FD))
(check-expect (handle-move (make-sw SN "down" FD))
              (make-sw (list (make-posn 3 5)) "down" FD))
(check-expect (handle-move (make-sw SN "up" FD))
              (make-sw (list (make-posn 3 3)) "up" FD))
(define (handle-move sw) 
  (cond
    [(string=? (sw-dir sw) "right") (make-sw
                                     (move-snake 1 0 (sw-snake sw))
                                     (sw-dir sw)
                                     (sw-food sw))]
    [(string=? (sw-dir sw) "left") (make-sw
                                     (move-snake -1 0 (sw-snake sw))
                                     (sw-dir sw)
                                     (sw-food sw))]
    [(string=? (sw-dir sw) "up") (make-sw
                                     (move-snake 0 -1 (sw-snake sw))
                                     (sw-dir sw)
                                     (sw-food sw))]
    [(string=? (sw-dir sw) "down") (make-sw
                                     (move-snake 0 1 (sw-snake sw))
                                     (sw-dir sw)
                                     (sw-food sw))]))

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
(check-expect (change-dir "right" (make-sw SN2 "left" FD))
              (make-sw SN2 "right" FD))
(check-expect (change-dir "down" (make-sw SN2 "down" FD))
              (make-sw SN2 "down" FD))
(define (change-dir d sw)
  (make-sw (sw-snake sw) d (sw-food sw)))

; SW KeyEvent -> SW
; Changes the direction of the snake when pressing arrow keys
(check-expect (handle-key (make-sw SN "right" FD) "right")
              (make-sw SN "right" FD))
(check-expect (handle-key (make-sw SN "right" FD) "left")
              (make-sw SN "right" FD))
(check-expect (handle-key (make-sw SN "right" FD) "down")
              (make-sw SN "down" FD))
(check-expect (handle-key (make-sw SN "right" FD) "up")
              (make-sw SN "up" FD))

(check-expect (handle-key (make-sw SN "left" FD) "left")
              (make-sw SN "left" FD))
(check-expect (handle-key (make-sw SN "left" FD) "right")
              (make-sw SN "left" FD))
(check-expect (handle-key (make-sw SN "left" FD) "down")
              (make-sw SN "down" FD))
(check-expect (handle-key (make-sw SN "left" FD) "up")
              (make-sw SN "up" FD))

(check-expect (handle-key (make-sw SN "up" FD) "up")
              (make-sw SN "up" FD))
(check-expect (handle-key (make-sw SN "up" FD) "down")
              (make-sw SN "up" FD))
(check-expect (handle-key (make-sw SN "up" FD) "right")
              (make-sw SN "right" FD))
(check-expect (handle-key (make-sw SN "up" FD) "left")
              (make-sw SN "left" FD))

(check-expect (handle-key (make-sw SN "down" FD) "down")
              (make-sw SN "down" FD))
(check-expect (handle-key (make-sw SN "down" FD) "up")
              (make-sw SN "down" FD))
(check-expect (handle-key (make-sw SN "down" FD) "right")
              (make-sw SN "right" FD))
(check-expect (handle-key (make-sw SN "down" FD) "left")
              (make-sw SN "left" FD))
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

; Snake -> Food 
; Creates a new random position for the food which
; was not the previous p and also does not cover the Snake (s)
(check-satisfied (food-create (list (make-posn 1 1))) not=-1-1?)
(define (food-create s)
  (food-check-create
     s (make-posn (random FIELD-SIZE) (random FIELD-SIZE))))
 
; Snake Food -> Food 
; generative recursion 
; Checks to make sure that the candidate position does not
; fall in place of the previously given position or any of the
; Snake's positions
(define (food-check-create s candidate)
  (if (member? candidate s)
      (food-create s)
      candidate))

; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

(define (game speed)
  (big-bang SW
    [to-draw render]
    [stop-when collision? render-game-over]
    [on-key handle-key]
    [on-tick tock speed]))