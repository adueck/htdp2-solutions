;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-101) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define ASPECT-RATIO (/ 1 1))
(define WIDTH 400)
(define HEIGHT (* WIDTH ASPECT-RATIO))
(define BACKGROUND
  (above (empty-scene WIDTH (* HEIGHT 0.8) "blue")
         (empty-scene WIDTH (* HEIGHT 0.2) "green")))

(define TANK-HEIGHT (* WIDTH 0.08))
(define TANK-WIDTH (* TANK-HEIGHT 3))
(define TANK (rectangle
              TANK-WIDTH
              TANK-HEIGHT
              "solid" "brown"))
(define TANK-DELTA-X 3)
(define TANK-MAX-X (+ WIDTH (/ TANK-WIDTH 2)))
(define TANK-MIN-X (- 0 (/ TANK-WIDTH 2)))

(define UFO (ellipse
             (* WIDTH 0.18)
             (* WIDTH 0.08)
             "solid" "grey"))
(define UFO-WOBBLE-X 5)
(define UFO-DELTA-Y 1)

(define MISSILE (rectangle
                 (* TANK-WIDTH 0.075)
                 (* TANK-HEIGHT 0.75)
                 "solid" "red"))
(define MISSILE-DELTA-Y 5)

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location


; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)


(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

(check-expect
 (si-render
   (make-sigs (make-posn 20 10) (make-tank 28 -3) #false))
 (place-images/align
   (list UFO TANK)
   (list (make-posn 20 10)
         (make-posn 28 HEIGHT))
   "center" "bottom"
   BACKGROUND))

(check-expect
  (si-render (make-sigs (make-posn 20 10)
            (make-tank 28 -3)
            (make-posn 28 (- HEIGHT TANK-HEIGHT))))
   (place-images/align
   (list UFO TANK MISSILE)
   (list (make-posn 20 10)
         (make-posn 28 HEIGHT)
         (make-posn 28 (- HEIGHT TANK-HEIGHT)))
   "center" "bottom"
   BACKGROUND))

(check-expect
 (si-render
   (make-sigs (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103)))
  (place-images/align
   (list UFO TANK MISSILE)
   (list (make-posn 20 100)
         (make-posn 100 HEIGHT)
         (make-posn 22 103))
   "center" "bottom"
   BACKGROUND))

; SIGS.v2 -> Image
; renders the given game state on top of BACKGROUND
; for examples/tests see above
(define (si-render s)
  (tank-render
    (sigs-tank s)
    (ufo-render (sigs-ufo s)
      (missile-render (sigs-missile s)
            BACKGROUND))))

; Tank Image -> Image
; adds t to the given image im
(check-expect (tank-render (make-tank 40 4) BACKGROUND)
              (place-image/align
               TANK 40 HEIGHT "center" "bottom"
               BACKGROUND))
(define (tank-render t im)
  (place-image/align
   TANK (tank-loc t) HEIGHT "center" "bottom"
   im))

; UFO Image -> Image
; adds u to the given image im
(check-expect (ufo-render (make-posn 30 40)
                          BACKGROUND)
              (place-image/align
               UFO 30 40 "center" "bottom"
               BACKGROUND))
(define (ufo-render u im)
  (place-image/align
    UFO (posn-x u) (posn-y u)
    "center" "bottom"
    im))

; MissileOrNot Image -> Image
; adds m to the given image im
(check-expect (missile-render (make-posn 20 60)
                          BACKGROUND)
              (place-image/align
               MISSILE 20 60 "center" "bottom"
               BACKGROUND))
(check-expect (missile-render #false BACKGROUND) BACKGROUND)
(define (missile-render m im)
  (cond
    [(boolean? m) im]
    [(posn? m) (place-image/align
                MISSILE (posn-x m) (posn-y m)
                "center" "bottom"
                im)]))

; SIGS.v2 -> Boolean
; checks to see if the game is over
; game is over when the ufo lands of the missile hits it
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 (+ (* HEIGHT 0.8) 2))
                (make-tank 28 -3)
                #false))
              #true)
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)
                #false))
              #false)
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 10)
                (make-tank 28 -3)
                (make-posn 28 (- HEIGHT TANK-HEIGHT))))
              #false)
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 10)
                (make-tank 28 -3)
                (make-posn 20 (- 9 (/ (image-height UFO) 2)))))
              #true)
(define (si-game-over? s)
  (or (ufo-landed? (sigs-ufo s))
   (ufo-hit? (sigs-ufo s) (sigs-missile s))))

; UFO -> Boolean
; Checks to see if the UFO has landed
(check-expect (ufo-landed? (make-posn 20 (+ (* HEIGHT 0.8) 2)))
              #true)
(check-expect (ufo-landed? (make-posn 20 (- (* HEIGHT 0.8) 10)))
              #false)
(define (ufo-landed? u)
  (<= (+ (* HEIGHT 0.8) 2) (posn-y u)))

; UFO MissileOrNot -> Boolean
; Checks to see if a missile has hit the UFO
(check-expect (ufo-hit? (make-posn 20 10) (make-posn 20 (- 9 (/ (image-height UFO) 2))))
              #true)
(check-expect (ufo-hit? (make-posn 20 10) (make-posn 28 (- HEIGHT TANK-HEIGHT)))
              #false)
(check-expect (ufo-hit? (make-posn 20 10) #false)
              #false)
(define (ufo-hit? u m)
  (cond
    [(boolean? m) #false] 
    [(posn? m) (and
   (<= (abs (- (posn-x u) (posn-x m))) (/ (image-width UFO) 2))
   (<= (abs (- (posn-y u) (+ (posn-y m) (/ (image-height MISSILE) 2)))) (image-height UFO)))]))

; SIGS.v2 -> Image
; Displays the GAME OVER screen
(check-expect (si-render-final
               (make-sigs
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)
                #false))
              (place-image/align
    (text "GAME OVER" 20 "red") (/ HEIGHT 2) (/ WIDTH 2)
    "center" "center"
    (si-render (make-sigs
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)
                #false))))
(define (si-render-final si)
  (place-image/align
    (text "GAME OVER" 20 "red") (/ HEIGHT 2) (/ WIDTH 2)
    "center" "center"
    (si-render si)))

; SIGS.v2 -> SIGS.v2
; Moves the objects for each tick of the clock
(check-random
 (si-move (make-sigs
            (make-posn 20 40)
            (make-tank 28 -3)
            #false))
 (si-move-proper (make-sigs
            (make-posn 20 40)
            (make-tank 28 -3)
            #false) (random UFO-WOBBLE-X)))
(define (si-move w)
  (si-move-proper w (random UFO-WOBBLE-X)))

; SIGS.v2 Number -> SIGS.v2 
; moves the space-invader objects predictably by delta
; TODO: Implement moving x ufo
(check-expect (si-move-proper
               (make-sigs
                (make-posn 20 40)
                (make-tank 28 -3)
                #false) 4)
              (make-sigs
               (move-ufo (make-posn 20 40) 4)
               (move-tank (make-tank 28 -3))
               #false))
(check-expect (si-move-proper
               (make-sigs
                (make-posn 20 10)
                (make-tank 28 -3)
                (make-posn 28 50)) 4)
              (make-sigs
               (move-ufo (make-posn 20 10) 4)
               (move-tank (make-tank 28 -3))
               (move-missile (make-posn 28 50))))
(define (si-move-proper w delta)
  (make-sigs
    (move-ufo (sigs-ufo w) delta)
    (move-tank (sigs-tank w))
    (move-missile (sigs-missile w))))

; UFO -> UFO
; moves the ufo for each tick of the clock
; moves the x axis based on the random number given
; also makes sure the ufo doesn't move to far right or left
(check-expect (move-ufo
               (make-posn 20 40) 3)
              (make-posn 17 (+ 40 UFO-DELTA-Y)))
(check-expect (move-ufo
               (make-posn 20 40) 4)
              (make-posn 24 (+ 40 UFO-DELTA-Y)))
(check-expect (move-ufo
               (make-posn 0 40) 3)
              (make-posn 0 (+ 40 UFO-DELTA-Y)))
(check-expect (move-ufo
               (make-posn WIDTH 40) 4)
              (make-posn WIDTH (+ 40 UFO-DELTA-Y)))
(define (move-ufo u n)
  (make-posn
    (min WIDTH (max 0 (cond
     [(= (modulo n 2) 0) (+ (posn-x u) n)]
     [else (- (posn-x u) n)])))
   (+ (posn-y u) UFO-DELTA-Y)))

; Tank -> Tank
; moves the tank for each tick of the clock
(check-expect (move-tank
               (make-tank 20 -3))
              (make-tank 17 -3))
; tank moving off to right
(check-expect (move-tank
               (make-tank TANK-MAX-X 3))
               (make-tank (+ TANK-MIN-X 3) 3))
; tank moving off to left
(check-expect (move-tank
               (make-tank TANK-MIN-X -3))
               (make-tank (- TANK-MAX-X 3) -3))
(define (move-tank t)
  (make-tank
   (+
    (cond
      [(>= (tank-loc t) TANK-MAX-X) TANK-MIN-X]
      [(<= (tank-loc t) TANK-MIN-X) TANK-MAX-X]
      [else (tank-loc t)])
    (tank-vel t))
   (tank-vel t)))

; MissileOrNot -> MissileOrNot
; moves the missile for each tick of the clock
(check-expect (move-missile
               (make-posn 20 40))
               (make-posn 20 (- 40 MISSILE-DELTA-Y)))
(check-expect (move-missile
               #false) #false)
(define (move-missile m)
  (cond
    [(boolean? m) #false]
    [(posn? m) (make-posn
                (posn-x m)
                (- (posn-y m) MISSILE-DELTA-Y))]))

; SIGS.v2 KeyEvent -> SIGS.v2
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 -3) #false) "right")
 (set-tank-direction (make-sigs (make-posn 20 10) (make-tank 28 -3) #false) "right"))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 -3) #false) "left")
 (set-tank-direction (make-sigs (make-posn 20 10) (make-tank 28 -3) #false) "left"))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 3) #false) "f")
 (make-sigs (make-posn 20 10) (make-tank 28 3) #false))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 3) #false) " ")
 (fire-missile (make-sigs (make-posn 20 10) (make-tank 28 3) #false)))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 3) (make-posn 28 (- HEIGHT TANK-HEIGHT))) " ")
 (make-sigs (make-posn 20 10) (make-tank 28 3) (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(define (si-control w ke)
  (cond
    [(or
      (string=? "right" ke)
      (string=? "left" ke))
       (set-tank-direction w ke)]
    [(string=? " " ke) (fire-missile w)]
    [else w]))

; A Direction is one of the following Strings:
; - "right"
; - "left"

; SIGS.v2 Direction -> SIGS.v2
; ensures the tank is moving in the desired direction
(check-expect
 (set-tank-direction
               (make-sigs (make-posn 20 10) (make-tank 28 -3) #false) "right")
 (make-sigs (make-posn 20 10) (adjust-tank-direction (make-tank 28 -3) "right") #false))
(check-expect
 (set-tank-direction
               (make-sigs (make-posn 20 10) (make-tank 28 -3) (make-posn 20 20)) "right")
 (make-sigs (make-posn 20 10) (adjust-tank-direction (make-tank 28 -3) "right") (make-posn 20 20)))
(define (set-tank-direction w d)
  (make-sigs
   (sigs-ufo w)
   (adjust-tank-direction (sigs-tank w) d)
   (sigs-missile w)))

; Tank Direction -> Tank
; Sets the direction for a tank
(check-expect (adjust-tank-direction (make-tank 20 -3) "right")
              (make-tank 20 3))
(check-expect (adjust-tank-direction (make-tank 20 -3) "left")
              (make-tank 20 -3))
(check-expect (adjust-tank-direction (make-tank 20 3) "right")
              (make-tank 20 3))
(check-expect (adjust-tank-direction (make-tank 20 3) "left")
              (make-tank 20 -3))
(define (adjust-tank-direction t d)
  (cond
    [(string=? "right" d)
       (make-tank (tank-loc t) (abs (tank-vel t)))]
    [else (make-tank (tank-loc t) (- (abs (tank-vel t)) (* (abs (tank-vel t)) 2)))]))

; Sigs.v2 -> Sigs.v2
; fires a missile if none is present
(check-expect
 (fire-missile (make-sigs (make-posn 20 10) (make-tank 28 3) #false))
 (make-sigs (make-posn 20 10) (make-tank 28 3) (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(check-expect
 (fire-missile (make-sigs (make-posn 20 10) (make-tank 28 3) (make-posn 40 40)))
 (make-sigs (make-posn 20 10) (make-tank 28 3) (make-posn 40 40)))
(define (fire-missile w)
  (make-sigs
   (sigs-ufo w)
   (sigs-tank w)
   (cond
     [(boolean? (sigs-missile w)) (make-posn (tank-loc (sigs-tank w)) (- HEIGHT TANK-HEIGHT))]
     [(posn? (sigs-missile w)) (sigs-missile w)])))

(define (tank-game w)
  (big-bang w
    [on-key si-control]
    [to-draw si-render]
    [on-tick si-move]
    [stop-when si-game-over? si-render-final]))
