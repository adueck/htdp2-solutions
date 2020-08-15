;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-224-full-ufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define-struct sigs [ufo tank missiles])
; A SIGS is a structure:
;   (make-sigs UFO Tank List-of-Missiles)
; interpretation represents the complete state of a
; space invader game
 
; A List-of-Missiles is one of: 
; – '()
; – (cons Missile '())
; interpretation a list of the coordinates of each
; missile fired, if any

; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)

; A Direction is one of the following Strings:
; - "right"
; - "left"

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick
(define missile-fired (make-posn 28 (- HEIGHT TANK-HEIGHT)))
(define missile-flying (make-posn 20 (- HEIGHT (* TANK-HEIGHT 2))))
(define empty-missiles '())
(define two-missiles (list missile-flying missile-fired))
(define ufo-ex (make-posn 20 10))
(define tank-ex (make-tank 28 5))
(define fresh-sigs (make-sigs ufo-ex tank-ex empty-missiles))
(define fired-sigs (make-sigs ufo-ex tank-ex two-missiles))

(check-expect (si-render fresh-sigs)
              (place-image/align
               UFO 20 10 "center" "bottom"
               (place-image/align
                TANK 28 HEIGHT "center" "bottom"
                BACKGROUND)))
(check-expect (si-render fired-sigs)
              (place-image/align
               UFO 20 10 "center" "bottom"
               (place-image/align
                TANK 28 HEIGHT "center" "bottom"
                (place-image/align
                 MISSILE 28 (- HEIGHT TANK-HEIGHT) "center" "bottom"
                 (place-image/align
                  MISSILE 20 (- HEIGHT (* TANK-HEIGHT 2)) "center" "bottom"
                  BACKGROUND)))))
; SIGS -> Image
; renders the given game state on top of BACKGROUND
; for examples/tests see above
(define (si-render s)
  (tank-render
    (sigs-tank s)
    (ufo-render (sigs-ufo s)
      (missiles-render (sigs-missiles s)
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

; Missiles Image -> Image
; adds m to the given image im
(check-expect (missiles-render two-missiles BACKGROUND)
              (place-image/align
               MISSILE 28 (- HEIGHT TANK-HEIGHT)
               "center" "bottom"
               (place-image/align
                MISSILE 20 (- HEIGHT (* TANK-HEIGHT 2))
                "center" "bottom"
                BACKGROUND)))
(check-expect (missiles-render empty-missiles BACKGROUND) BACKGROUND)
(define (missiles-render m im)
  (cond
    [(empty? m) BACKGROUND]
    [else (place-image/align
            MISSILE (posn-x (first m)) (posn-y (first m))
            "center" "bottom"
            (missiles-render (rest m) im))]))

; SIGS.v2 -> Boolean
; checks to see if the game is over
; game is over when the ufo lands of the missile hits it
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 (+ (* HEIGHT 0.8) 2))
                (make-tank 28 -3)
                '()))
              #true)
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)
                '()))
              #false)
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 10)
                (make-tank 28 -3)
                (list (make-posn 28 (- HEIGHT TANK-HEIGHT)))))
              #false)
(check-expect (si-game-over?
               (make-sigs
                (make-posn 20 10)
                (make-tank 28 -3)
                (list (make-posn 20 (- 9 (/ (image-height UFO) 2))))))
              #true)
(define (si-game-over? s)
  (or (ufo-landed? (sigs-ufo s))
   (ufo-hit? (sigs-ufo s) (sigs-missiles s))))

; UFO -> Boolean
; Checks to see if the UFO has landed
(check-expect (ufo-landed? (make-posn 20 (+ (* HEIGHT 0.8) 2)))
              #true)
(check-expect (ufo-landed? (make-posn 20 (- (* HEIGHT 0.8) 10)))
              #false)
(define (ufo-landed? u)
  (<= (+ (* HEIGHT 0.8) 2) (posn-y u)))

; UFO Missiles -> Boolean
; Checks to see if the UFO is hit by any of the missiles
(check-expect (ufo-hit? (make-posn 20 10)
                        (list (make-posn 20 (- 9 (/ (image-height UFO) 2)))))
              #true)
(check-expect (ufo-hit? (make-posn 20 10)
                        (list (make-posn 28 (- HEIGHT TANK-HEIGHT))))
              #false)
(check-expect (ufo-hit? (make-posn 20 10) '())
              #false)
(define (ufo-hit? u ml)
  (cond
    [(empty? ml) #false] 
    [else (or
           (missile-hit? u (first ml))
           (ufo-hit? u (rest ml)))]))

; UFO Missile -> Boolean
; Checks to see if a single missile has hit the UFO
(define (missile-hit? u m)
  (and
    (<= (abs (- (posn-x u) (posn-x m))) (/ (image-width UFO) 2))
    (<= (abs (- (posn-y u) (+ (posn-y m) (/ (image-height MISSILE) 2))))
        (image-height UFO))))

; SIGS -> Image
; Displays the GAME OVER screen
(check-expect (si-render-final
               (make-sigs
                (make-posn 20 (- (* HEIGHT 0.8) 5))
                (make-tank 28 -3)
                '()))
              (place-image/align
               (text "GAME OVER" 20 "red") (/ HEIGHT 2) (/ WIDTH 2)
               "center" "center"
               (si-render (make-sigs
                           (make-posn 20 (- (* HEIGHT 0.8) 5))
                           (make-tank 28 -3)
                           '()))))
(define (si-render-final si)
  (place-image/align
    (text "GAME OVER" 20 "red") (/ HEIGHT 2) (/ WIDTH 2)
    "center" "center"
    (si-render si)))

; SIGS -> SIGS
; Moves the objects for each tick of the clock
(check-random
  (si-move (make-sigs
            (make-posn 20 40)
            (make-tank 28 -3)
            '()))
  (si-move-proper (make-sigs
            (make-posn 20 40)
            (make-tank 28 -3)
            '()) (random UFO-WOBBLE-X)))
(define (si-move w)
  (si-move-proper w (random UFO-WOBBLE-X)))

; SIGS.v2 Number -> SIGS.v2
; moves the space-invader objects predictably by delta
; TODO: Implement moving x ufo
(check-expect (si-move-proper
               (make-sigs
                (make-posn 20 40)
                (make-tank 28 -3)
                '()) 4)
              (make-sigs
               (move-ufo (make-posn 20 40) 4)
               (move-tank (make-tank 28 -3))
               '()))
(check-expect (si-move-proper
               (make-sigs
                (make-posn 20 10)
                (make-tank 28 -3)
                (list (make-posn 28 50))) 4)
              (make-sigs
               (move-ufo (make-posn 20 10) 4)
               (move-tank (make-tank 28 -3))
               (move-missiles (list (make-posn 28 50)))))
(define (si-move-proper w delta)
  (make-sigs
    (move-ufo (sigs-ufo w) delta)
    (move-tank (sigs-tank w))
    (move-missiles (sigs-missiles w))))

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

; Missiles -> Missiles
; moves the missiles for each tick of the clock
(check-expect (move-missiles
               (list (make-posn 20 40)))
              (list (make-posn 20 (- 40 MISSILE-DELTA-Y))))
(check-expect (move-missiles
               '()) '())
(define (move-missiles m)
  (cond
    [(empty? m) '()]
    [else (cons (move-missile (first m))
                (move-missiles (rest m)))]))

; Missile -> Missile
; moves a missile for each tick of the clock
(check-expect (move-missile
               (make-posn 20 40))
               (make-posn 20 (- 40 MISSILE-DELTA-Y)))
(define (move-missile m)
  (make-posn
   (posn-x m)
   (- (posn-y m) MISSILE-DELTA-Y)))

; SIGS KeyEvent -> SIGS
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 -3) '()) "right")
 (set-tank-direction (make-sigs (make-posn 20 10) (make-tank 28 -3) '()) "right"))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 -3) '()) "left")
 (set-tank-direction (make-sigs (make-posn 20 10) (make-tank 28 -3) '()) "left"))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 3) '()) "f")
 (make-sigs (make-posn 20 10) (make-tank 28 3) '()))
(check-expect
 (si-control (make-sigs (make-posn 20 10) (make-tank 28 3) '()) " ")
 (fire-missile (make-sigs (make-posn 20 10) (make-tank 28 3) '())))
(define (si-control w ke)
  (cond
    [(or
      (string=? "right" ke)
      (string=? "left" ke))
       (set-tank-direction w ke)]
    [(string=? " " ke) (fire-missile w)]
    [else w]))

; SIGS.v2 Direction -> SIGS.v2
; ensures the tank is moving in the desired direction
(check-expect
 (set-tank-direction
  (make-sigs (make-posn 20 10) (make-tank 28 -3) '()) "right")
 (make-sigs (make-posn 20 10) (adjust-tank-direction (make-tank 28 -3) "right") '()))
(check-expect
 (set-tank-direction
  (make-sigs (make-posn 20 10) (make-tank 28 -3) (list (make-posn 20 20))) "right")
 (make-sigs (make-posn 20 10) (adjust-tank-direction (make-tank 28 -3) "right")
            (list (make-posn 20 20))))
(define (set-tank-direction w d)
  (make-sigs
   (sigs-ufo w)
   (adjust-tank-direction (sigs-tank w) d)
   (sigs-missiles w)))

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

; Sigs -> Sigs
; fires a missile
(check-expect
 (fire-missile (make-sigs
                (make-posn 20 10)
                (make-tank 28 3)
                '()))
 (make-sigs (make-posn 20 10)
            (make-tank 28 3)
            (list (make-posn 28 (- HEIGHT TANK-HEIGHT)))))
(check-expect
 (fire-missile (make-sigs (make-posn 20 10)
                          (make-tank 28 3)
                          (list (make-posn 40 40))))
 (make-sigs
  (make-posn 20 10)
  (make-tank 28 3)
  (list (make-posn 28 (- HEIGHT TANK-HEIGHT))
        (make-posn 40 40))))
(define (fire-missile w)
  (make-sigs
   (sigs-ufo w)
   (sigs-tank w)
   (cons
    (make-posn (tank-loc (sigs-tank w)) (- HEIGHT TANK-HEIGHT))
    (sigs-missiles w))))

(define (tank-game w)
  (big-bang w
    [on-key si-control]
    [to-draw si-render]
    [on-tick si-move]
    [stop-when si-game-over? si-render-final]))
