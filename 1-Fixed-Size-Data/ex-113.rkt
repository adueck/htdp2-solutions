;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-113) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; A Coordinate is one of: 
; – a NegativeNumber 
; interpretation on the y axis, distance from top
; – a PositiveNumber 
; interpretation on the x axis, distance from left
; – a Posn
; interpretation an ordinary Cartesian point

; Any -> Boolean
; is a an element of a Coordinate collection
(check-expect (coord? -3) #true)
(check-expect (coord? 2) #true)
(check-expect (coord? (make-posn 3 3)) #true)
(check-expect (coord? "text") #false)
(check-expect (coord? #false) #false)
(define (coord? a)
  (cond
    [(number? a) #true]
    [(posn? a) #true]
    [else #false]))

; A VAnimal is either
; – a VCat
; – a VCham

(define-struct vcham [xpos happiness color])
; A VCham is a structure
;  (make-vcham Number Happiness Color)

(define-struct vcat [xpos dir happiness])
; A VCat is a structure
;  (make-vcat Number Direction Happiness)

; A Color is one of the following Strings:
; - "red"
; - "blue"
; - "green"

; A Directon is one of the following Strings:
; - "left"
; - "right"

; Happiness is a Number between 0 and 100
; interpretation the percentage of the hapiness guage

; Any -> Boolean
; Is a a member of the VAnimal collection
(define (vanimal? a)
  (cond
    [(vcat? a) (and
                (number? (vcat-xpos a))
                (dir? (vcat-dir a))
                (happiness? (vcat-happiness a)))]
    [(vcham? a) (and
                 (number? (vcham-xpos a))
                 (happiness? (vcham-happiness a))
                 (color? (vcham-color a)))]
    [else #false]))

; Any -> Boolean
; Is a a Color
(check-expect (color? "red") #true)
(check-expect (color? "blue") #true)
(check-expect (color? "green") #true)
(check-expect (color? 7) #false)
(check-expect (color? "pink") #false)
(define (color? a)
  (cond
    [(string? a)
     (or
      (string=? a "red")
      (string=? a "blue")
      (string=? a "green"))]
    [else #false]))

; Any -> Boolean
; Is a a Direction
(check-expect (dir? "left") #true)
(check-expect (dir? "right") #true)
(check-expect (dir? "green") #false)
(check-expect (dir? 7) #false)
(define (dir? a)
  (cond
    [(string? a)
     (or
      (string=? a "left")
      (string=? a "right"))]
    [else #false]))

; Any -> Boolean
; Is a a Happiness
(check-expect (happiness? 30) #true)
(check-expect (happiness? 0) #true)
(check-expect (happiness? 100) #true)
(check-expect (happiness? 140) #false)
(check-expect (happiness? -20) #false)
(check-expect (happiness? "str") #false)
(define (happiness? a)
  (cond
    [(number? a) (and (>= a 0) (<= a 100))]
    [else #false]))