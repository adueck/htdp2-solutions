;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-100) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define DEMO-SCENE
  (place-images/align
   (list TANK UFO)
   (list (make-posn (* WIDTH 0.3) HEIGHT)
         (make-posn (* WIDTH 0.6) (* HEIGHT 0.2)))
   "center" "bottom"
   BACKGROUND))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

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

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; Any -> Boolean
; is a an element of a SIGS collection
(check-expect (SIGS? (make-aim (make-posn 2 2) (make-tank 2 2)))
              #true)
(check-expect (SIGS? (make-aim (make-posn 2 "a") (make-tank 2 2)))
              #false)
(check-expect (SIGS? (make-fired (make-posn 2 2) (make-tank 2 2) (make-posn 2 2)))
              #true)
(check-expect (SIGS? "text") #false)
(check-expect (SIGS? (make-posn 3 4)) #false)
(define (SIGS? a)
  (cond
    [(aim? a) (and
               (number? (posn-x (aim-ufo a)))
               (number? (posn-y (aim-ufo a)))
               (number? (tank-loc (aim-tank a)))
               (number? (tank-vel (aim-tank a))))]
    [(fired? a) (and
               (number? (posn-x (fired-ufo a)))
               (number? (posn-y (fired-ufo a)))
               (number? (tank-loc (fired-tank a)))
               (number? (tank-vel (fired-tank a)))
               (number? (posn-x (fired-missile a)))
               (number? (posn-y (fired-missile a))))]
    [else #false]))


(check-expect
 (si-render
   (make-aim (make-posn 20 10) (make-tank 28 -3)))
 (place-images/align
   (list UFO TANK)
   (list (make-posn 20 10)
         (make-posn 28 HEIGHT))
   "center" "bottom"
   BACKGROUND))

(check-expect
  (si-render (make-fired (make-posn 20 10)
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
   (make-fired (make-posn 20 100)
            (make-tank 100 3)
            (make-posn 22 103)))
  (place-images/align
   (list UFO TANK MISSILE)
   (list (make-posn 20 100)
         (make-posn 100 HEIGHT)
         (make-posn 22 103))
   "center" "bottom"
   BACKGROUND))

; SIGS -> Image
; renders the given game state on top of BACKGROUND
; for examples/tests see above
(define (si-render s)
  (cond
    [(aim? s)
      (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
      (tank-render
        (fired-tank s)
        (ufo-render (fired-ufo s)
          (missile-render (fired-missile s)
            BACKGROUND)))]))

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

; Missile Image -> Image
; adds m to the given image im
(check-expect (missile-render (make-posn 20 60)
                          BACKGROUND)
              (place-image/align
               MISSILE 20 60 "center" "bottom"
               BACKGROUND))
(define (missile-render m im)
   (place-image/align
    MISSILE (posn-x m) (posn-y m)
    "center" "bottom"
    im))

; SIGS -> Boolean
; checks to see if the game is over
; game is over when the ufo lands of the missile hits it
(check-expect (si-game-over?
               (make-aim
                (make-posn 20 (+ (* HEIGHT 0.8) 2))
                (make-tank 28 -3)))
              #true)
(check-expect (si-game-over?
               (make-aim
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)))
              #false)
(check-expect (si-game-over?
               (make-fired
                (make-posn 20 10)
                (make-tank 28 -3)
                (make-posn 28 (- HEIGHT TANK-HEIGHT))))
              #false)
(check-expect (si-game-over?
               (make-fired
                (make-posn 20 10)
                (make-tank 28 -3)
                (make-posn 20 (+ (image-height UFO) 5))))
              #true)
(define (si-game-over? s)
  (cond
    [(aim? s)
       (ufo-landed? (aim-ufo s))]
    [else
       (or (ufo-landed? (fired-ufo s))
           (ufo-hit? (fired-ufo s) (fired-missile s)))]))

; UFO -> Boolean
; Checks to see if the UFO has landed
(check-expect (ufo-landed? (make-posn 20 (+ (* HEIGHT 0.8) 2)))
              #true)
(check-expect (ufo-landed? (make-posn 20 (- (* HEIGHT 0.8) 10)))
              #false)
(define (ufo-landed? u)
  (<= (+ (* HEIGHT 0.8) 2) (posn-y u)))

; UFO Missile -> Boolean
; Checks to see if a missile has hit the UFO
(check-expect (ufo-hit? (make-posn 20 10) (make-posn 20 (+ (image-height UFO) 5)))
              #true)
(check-expect (ufo-hit? (make-posn 20 10) (make-posn 28 (- HEIGHT TANK-HEIGHT)))
              #false)
(define (ufo-hit? u m)
  (and
   (<= (abs (- (posn-x u) (posn-x m))) (image-width UFO))
   (<= (abs (- (posn-y u) (posn-y m))) (image-height UFO))))

; SIGS -> Image
; Displays the GAME OVER screen
(check-expect (si-render-final (make-aim
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)))
              (place-image/align
    (text "GAME OVER" 20 "red") (/ HEIGHT 2) (/ WIDTH 2)
    "center" "center"
    (si-render (make-aim
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)))))
(define (si-render-final si)
  (place-image/align
    (text "GAME OVER" 20 "red") (/ HEIGHT 2) (/ WIDTH 2)
    "center" "center"
    (si-render si)))

; SIGS -> SIGS
; Moves the objects for each tick of the clock
(check-random
 (si-move (make-aim
            (make-posn 20 40)
            (make-tank 28 -3)))
 (si-move-proper (make-aim
            (make-posn 20 40)
            (make-tank 28 -3)) (random UFO-WOBBLE-X)))
(define (si-move w)
  (si-move-proper w (random UFO-WOBBLE-X)))

; SIGS Number -> SIGS 
; moves the space-invader objects predictably by delta
; TODO: Implement moving x ufo
(check-expect (si-move-proper
               (make-aim
                (make-posn 20 40)
                (make-tank 28 -3)) 4)
              (make-aim
               (move-ufo (make-posn 20 40) 4)
               (move-tank (make-tank 28 -3))))
(check-expect (si-move-proper
               (make-fired
                (make-posn 20 10)
                (make-tank 28 -3)
                (make-posn 28 50)) 4)
              (make-fired
               (move-ufo (make-posn 20 10) 4)
               (move-tank (make-tank 28 -3))
               (move-missile (make-posn 28 50))))
(define (si-move-proper w delta)
  (cond
    [(aim? w)
       (make-aim
        (move-ufo (aim-ufo w) delta)
        (move-tank (aim-tank w)))]
    [else
      (make-fired
        (move-ufo (fired-ufo w) delta)
        (move-tank (fired-tank w))
        (move-missile (fired-missile w)))]))

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

; Missile -> Missile
; moves the missile for each tick of the clock
(check-expect (move-missile
               (make-posn 20 40))
               (make-posn 20 (- 40 MISSILE-DELTA-Y)))
(define (move-missile m)
  (make-posn
   (posn-x m)
   (- (posn-y m) MISSILE-DELTA-Y)))

; SIGS KeyEvent -> SIGS
(check-expect
 (si-control (make-aim (make-posn 20 10) (make-tank 28 -3)) "right")
 (set-tank-direction (make-aim (make-posn 20 10) (make-tank 28 -3)) "right"))
(check-expect
 (si-control (make-aim (make-posn 20 10) (make-tank 28 -3)) "left")
 (set-tank-direction (make-aim (make-posn 20 10) (make-tank 28 -3)) "left"))
(check-expect
 (si-control (make-aim (make-posn 20 10) (make-tank 28 3)) "f")
 (make-aim (make-posn 20 10) (make-tank 28 3)))
(check-expect
 (si-control (make-aim (make-posn 20 10) (make-tank 28 3)) " ")
 (fire-missile (make-aim (make-posn 20 10) (make-tank 28 3))))
(check-expect
 (si-control (make-fired (make-posn 20 10) (make-tank 28 3) (make-posn 28 (- HEIGHT TANK-HEIGHT))) " ")
 (make-fired (make-posn 20 10) (make-tank 28 3) (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(define (si-control w ke)
  (cond
    [(or
      (string=? "right" ke)
      (string=? "left" ke))
       (set-tank-direction w ke)]
    [(string=? " " ke) (if (aim? w) (fire-missile w) w)]
    [else w]))

; A Direction is one of the following Strings:
; - "right"
; - "left"

; SIGS Direction -> SIGS
; ensures the tank is moving in the desired direction
(check-expect
 (set-tank-direction
               (make-aim (make-posn 20 10) (make-tank 28 -3)) "right")
 (make-aim (make-posn 20 10) (adjust-tank-direction (make-tank 28 -3) "right")))
(check-expect
 (set-tank-direction
               (make-fired (make-posn 20 10) (make-tank 28 -3) (make-posn 20 20)) "right")
 (make-fired (make-posn 20 10) (adjust-tank-direction (make-tank 28 -3) "right") (make-posn 20 20)))
(define (set-tank-direction w d)
  (cond
    [(aim? w) (make-aim
               (aim-ufo w)
               (adjust-tank-direction (aim-tank w) d))]
    [else (make-fired
                 (fired-ufo w)
                 (adjust-tank-direction (fired-tank w) d)
                 (fired-missile w))]))

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

; Aim -> Fired
; fires a missile from an aimed state
(check-expect
 (fire-missile (make-aim (make-posn 20 10) (make-tank 28 3)))
 (make-fired (make-posn 20 10) (make-tank 28 3) (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(define (fire-missile w)
  (make-fired
   (aim-ufo w)
   (aim-tank w)
   (make-posn (tank-loc (aim-tank w)) (- HEIGHT TANK-HEIGHT))))

(define (tank-game w)
  (big-bang w
    [check-with SIGS?]
    [on-key si-control]
    [to-draw si-render]
    [on-tick si-move]
    [stop-when si-game-over? si-render-final]))
