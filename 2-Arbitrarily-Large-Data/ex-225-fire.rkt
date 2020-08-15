;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-225-fire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define ASPECT-RATIO (/ 1 1))
(define WIDTH 400)
(define HEIGHT (* WIDTH ASPECT-RATIO))
(define BACKGROUND
  (above (empty-scene WIDTH (* HEIGHT 0.8) "white");(make-color 186 232 255))
         (empty-scene WIDTH (* HEIGHT 0.2) "green")))

(define WING
  (ellipse (* WIDTH 0.25) (* WIDTH 0.03) "solid" "grey"))
(define FUSULAGE
  (circle (* WIDTH 0.0325) "solid" "grey"))
(define PLANE
  (overlay FUSULAGE WING))
(define PLANE-Y (* HEIGHT 0.1))
(define FIRE-Y (* HEIGHT 0.8))
(define PLANE-DELTA-X 3)
(define PLANE-LEFT
  (rotate 3 PLANE))
(define PLANE-RIGHT
  (rotate -3 PLANE))
(define WATER
  (ellipse (* WIDTH 0.035) (* WIDTH 0.05) "solid" "blue"))
(define WATER-DELTA-Y 5)
(define WATER-Y-START (+ PLANE-Y (/ (image-height PLANE) 2)))
(define FIRE (overlay/offset (ellipse (* WIDTH 0.03) (* WIDTH 0.04) "solid" "yellow")
                  0 (- (* WIDTH 0.01))
                  (ellipse (* WIDTH 0.05) (* WIDTH 0.065) "solid" "red")))
(define MAX-FIRES 8)
(define FIRE-CHANCE 300)
(define EXTINGUISH-TOLERANCE (/ (image-width FIRE) 2))
; A Plane is a structure:
(define-struct pln [x dir])
;  (make-pln Number Direction)
; interpretation the x-coordiate of the center of the place
; and the dir Direction its flying

; A Direction is one of the following Strings:
; - "left"
; - "right"
; interpretation the direction that a plane is flying

; A Water is a Posn
; interpretation the position of a payload of water dropped by the plane

; A List-of-Waters is one of the following:
; - '()
; - (Water List-of-Waters)
; interpretation a list of posns representing the positions of the
; payloads dropped by the plane

; A Fire is a Number
; interpretation the x-coordinate of the center of a fire

; A List-of-Fires is one of the following:
; - '()
; - (Fire List-of-Fires)
; interpretation a list of all the x-coordinates for all the fires burning

; A Time is a Number
; interpretation the amount of ticks left in a game (28 ticks per second)

; A FireWorld (FW for short) is a structure
(define-struct fw [plane waters fires time])
;  (make-fw [Plane Waters Fires Time])
(define empty-sky (make-fw (make-pln 100 "right") '() '() 28))
(define some-fires (make-fw (make-pln 150 "right") '() (list 50 180 240) (* 30 10)))
(define dropping-waters (make-fw
                         (make-pln 200 "left")
                         (list (make-posn 200 230) (make-posn 300 300))
                         '()
                         57))

; FW -> Image
; Renders the current world to Image
(check-expect (render empty-sky)
              (place-image/align (text "2" 18 "black") 10 10 "left" "top"
                                 (place-image PLANE-RIGHT 100 PLANE-Y BACKGROUND)))
(check-expect (render some-fires)
              (place-image/align (text "11" 18 "black") 10 10 "left" "top"
                                 (place-image PLANE-RIGHT 150 PLANE-Y
                                              (place-image FIRE 50 FIRE-Y
                                                           (place-image FIRE 180 FIRE-Y
                                                                        (place-image FIRE 240 FIRE-Y BACKGROUND))))))
(check-expect (render dropping-waters)
              (place-image/align (text "3" 18 "black") 10 10 "left" "top"
                                 (place-image PLANE-LEFT 200 PLANE-Y
                                              (place-image WATER 200 230
                                                           (place-image WATER 300 300 BACKGROUND)))))
               
(define (render s)
  (render-time (fw-time s)
               (render-plane (fw-plane s)
                             (render-waters (fw-waters s)
                                            (render-fires
                                             (fw-fires s)
                                             BACKGROUND)))))

; FW -> Image
; Renders the game over scene
(check-expect (render-end (make-fw (make-pln 100 "right") '() '() 0))
              (place-image
               (text "SUCCESS" 20 "green")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render (make-fw (make-pln 100 "right") '() '() 0))))
(check-expect (render-end (make-fw (make-pln 100 "right") '() (list 2 10) 0))
              (place-image
               (text "FAIL" 20 "red")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render (make-fw (make-pln 100 "right") '() (list 2 10) 0))))
(define (render-end s)
  (place-image
   (cond
     [(= (length (fw-fires s)) 0) (text "SUCCESS" 20 "green")]
     [else (text "FAIL" 20 "red")])
   (/ WIDTH 2)
   (/ HEIGHT 2)
   (render s)))

; Time Image -> Image
; Renders the time left in seconds on a given scene
(define (render-time t im)
  (place-image/align
               (text (to-secs t) 18 "black") 10 10 "left" "top" im))

; Time -> String
; Gives the number of seconds left for a given Time left
(check-expect (to-secs 0) "0")
(check-expect (to-secs 28) "2")
(check-expect (to-secs 3) "1")
(check-expect (to-secs 57) "3")
(define (to-secs t)
  (cond
    [(= t 0) "0"]
    [else
     (number->string (add1 (floor (/ t 28))))]))

; Plane Image -> Image
; Renders the Plane on a given background
(define (render-plane p im)
  (place-image (cond
                 [(string=? (pln-dir p) "right") PLANE-RIGHT]
                 [(string=? (pln-dir p) "left") PLANE-LEFT])
               (pln-x p) PLANE-Y im))

; Waters Image -> Image
; Renders the Waters on a given background
(define (render-waters w im)
  (cond
    [(empty? w) im]
    [else (render-water (first w) (render-waters (rest w) im))]))

; Water Image -> Image
; Renders a given Water onto a given background
(define (render-water w im)
  (place-image WATER (posn-x w) (posn-y w) im))

; Fires Image -> Image
; Renders the Fires on a given background
(define (render-fires f im)
  (cond
   [(empty? f) im]
   [else (render-fire (first f) (render-fires (rest f) im))]))

; Fire Image -> Image
; Renders a given Fire on a given background
(define (render-fire f im)
  (place-image FIRE f FIRE-Y im))

; FW -> FW
; handles the move of the scene for each tick of the clock
(check-expect (tock empty-sky)
              (make-fw (move-plane (fw-plane empty-sky))
                       (fw-waters empty-sky)
                       (fw-fires empty-sky)
                       27))
(check-expect (tock dropping-waters)
              (make-fw (move-plane (fw-plane dropping-waters))
                       (move-waters (fw-waters dropping-waters))
                       (check-fires (fw-fires dropping-waters) (fw-waters dropping-waters))
                       56))
(define (tock s)
  (make-fw
   (move-plane (fw-plane s))
   (move-waters (fw-waters s))
   (check-fires (fw-fires s) (fw-waters s))
   (sub1 (fw-time s))))

; Waters -> Waters
; Moves a list of Waters down with gravity
(check-expect (move-waters '()) '())
(check-expect (move-waters (list (make-posn 30 (add1 FIRE-Y)))) '()) 
(check-expect (move-waters (list (make-posn 100 80) (make-posn 200 120)))
              (list
               (make-posn 100 (+ 80 WATER-DELTA-Y))
               (make-posn 200 (+ 120 WATER-DELTA-Y))))
(define (move-waters w)
  (cond
    [(empty? w) '()]
    [else (if
           (hit-ground? (first w))
           (move-waters (rest w))
           (cons (move-water (first w)) (move-waters (rest w))))]))

; Water -> Boolean
; Determines if a water has hit the ground
(check-expect (hit-ground? (make-posn 200 20)) #f)
(check-expect (hit-ground? (make-posn 100 (+ FIRE-Y 30))) #t)
(define (hit-ground? w)
  (>= (posn-y w) FIRE-Y))

; Water -> Water
; Move a single Water down with gravity
(check-expect (move-water (make-posn 20 10))
              (make-posn 20 (+ 10 WATER-DELTA-Y)))
(define (move-water w)
  (make-posn (posn-x w) (+ (posn-y w) WATER-DELTA-Y)))

; Plane -> Plane
; Moves the plane for every tick of the clock
(check-expect (move-plane (make-pln 100 "right")) (make-pln (+ 100 PLANE-DELTA-X) "right"))
(check-expect (move-plane (make-pln 100 "left")) (make-pln (- 100 PLANE-DELTA-X) "left"))
(define (move-plane p)
  (make-pln
   (modulo
    (+
    (pln-x p)
    (if (string=? (pln-dir p) "right")
        PLANE-DELTA-X
        (- PLANE-DELTA-X)))
    WIDTH)
   (pln-dir p)))

; Fires Waters -> Fires
; extinguishes any fires that have been hit by a Water in a list
(check-expect (check-fires '() '()) '())
(check-expect (check-fires (list 300 101) (list (make-posn 100 (+ FIRE-Y 30))))
              (list 300))
(define (check-fires f w)
  (rand-add-fire
   (cond
    [(empty? f) '()]
    [else (if (gets-hit? (first f) w)
              (check-fires (rest f) w)
              (cons (first f) (check-fires (rest f) w)))])))

; Fires -> Fires
; possibly randomly add a fire for every tick if there's not too many
(check-expect (rand-add-fire (make-list MAX-FIRES 7)) (make-list MAX-FIRES 7))
(check-random (rand-add-fire '()) (if (= (random FIRE-CHANCE) 1)
                                      (cons (make-new-fire '()) '())
                                      '()))
(define (rand-add-fire f)
  (if (and
       (= (random FIRE-CHANCE) 1)
       (< (length f) MAX-FIRES))
      (cons (make-new-fire f) f)
      f))

; Fires -> Fire
; create a new fire not overlapping with existing fires
(check-random (make-new-fire '()) (random WIDTH))
(define (make-new-fire f) (random WIDTH))

; Fire Waters -> Boolean
; Checks if any Water in a list hits a particular fire
(check-expect (gets-hit? 200 (list (make-posn 198 (+ FIRE-Y 20)))) #true)
(check-expect (gets-hit? 200 (list (make-posn 300 (+ FIRE-Y 20)))) #false)
(define (gets-hit? f w)
  (cond
    [(empty? w) #false]
    [else (or
           (water-hits-fire? (first w) f)
           (gets-hit? f (rest w)))]))

; Water Fire -> Boolean
; Cheche if a particular water hits a particular fire
(check-expect (water-hits-fire? (make-posn 50 10) 300) #f)
(check-expect (water-hits-fire? (make-posn 200 (+ FIRE-Y 20)) 201) #t)
(define (water-hits-fire? w f)
  (and
   (hit-ground? w)
   (<= (abs (- (posn-x w) f)) EXTINGUISH-TOLERANCE)))

; FW KeyEvent -> FW
; handles key presses
(check-expect (handle-key (make-fw (make-pln 50 "right") '() '() 28) "right")
              (make-fw (make-pln 50 "right") '() '() 28))
(check-expect (handle-key (make-fw (make-pln 50 "left") '() '() 28) "right")
              (make-fw (make-pln 50 "right") '() '() 28))
(check-expect (handle-key (make-fw (make-pln 50 "right") '() '() 28) "left")
              (make-fw (make-pln 50 "left") '() '() 28))
(check-expect (handle-key (make-fw (make-pln 50 "left") '() '() 28) "left")
              (make-fw (make-pln 50 "left") '() '() 28))
(check-expect (handle-key (make-fw (make-pln 50 "a") '() '() 28) "left")
              (make-fw (make-pln 50 "left") '() '() 28))
(check-expect (handle-key (make-fw (make-pln 50 "left") '() '() 28) " ")
              (make-fw
               (make-pln 50 "left")
               (drop-water (make-pln 50 "left") '())
               '() 28))
(define (handle-key s ke)
  (make-fw
   (steer (fw-plane s) ke)
   (if (string=? " " ke)
       (drop-water (fw-plane s) (fw-waters s))
       (fw-waters s))
   (fw-fires s)
   (fw-time s)))

; Plane Waters -> Waters
; Drops a new water
(check-expect (drop-water (make-pln 200 "right") '())
              (list (make-posn 200 WATER-Y-START)))
(define (drop-water p w)
  (cons (make-posn (pln-x p) WATER-Y-START) w))

; FW -> Boolean
; Checks if the time is up (game over)
(check-expect (game-over? (make-fw (make-pln 100 "right") '() '() 3)) #f)
(check-expect (game-over? (make-fw (make-pln 100 "right") '() '() 0)) #t)
(define (game-over? s)
  (<= (fw-time s) 0))

; Plane KeyEvent -> Plane
; handles directional input to the plane
(define (steer p ke)
  (make-pln
   (pln-x p)
   (cond
     [(key=? "right" ke) "right"]
     [(key=? "left" ke) "left"]
     [else (pln-dir p)])))

(define (game s)
  (big-bang (make-fw (make-pln 100 "right") '() '() (* 30 28))
    [to-draw render]
    [on-tick tock]
    [on-key handle-key]
    [stop-when game-over? render-end]))

