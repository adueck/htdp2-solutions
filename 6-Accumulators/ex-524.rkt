;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-524) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct ps [scene prev])
; a PuzzleState is a structure
;  (make-ps [Scene [List-of Scene]])
; interpretation a Scene describing the situation
; as well as a list of Scenes that it took to get to
; this point

(define-struct scene [left right boat])
; a Scene is a structure
;  (make-scene [People People BoatPosn])
; interpretation the People on both sides of the river
; as well as the position of the boat
; accumulator prev is the list of states that it took to get to that state

(define-struct people [miss cann])
; a People in a structure
;  (make-people [number number])
; interpretation the amount of Miss' and Cann's in a given group of people

; a BoatPosn is one of:
; - "left"
; - "right"
; interpretation the side of the river that the boat is on

(define initial-scene (make-scene (make-people 3 3)
                                  (make-people 0 0)
                                  "left"))
(define initial (make-ps
                 initial-scene
                 '()))
(define next-ps (make-ps
                (make-scene (make-people 2 3)
                            (make-people 1 0)
                            "right")
                (list (make-scene (make-people 3 3)
                             (make-people 0 0)
                             "left"))))

; PuzzleState -> Boolean
; Determines if all the people are on the right river bank
(check-expect (final? initial) #f)
(check-expect (final? (make-ps
                       (make-scene (make-people 0 0)
                                   (make-people 3 3)
                                   "right")
                       '())) #t)
(define (final? ps)
  (local ((define rs (scene-right (ps-scene ps))))
    (and (= 3 (people-miss rs))
         (= 3 (people-cann rs)))))

; [List-of PuzzleState] -> [List-of PuzzleState]
(define (create-next-states lop)
  (foldl (lambda (ps acc) (append
                           (next-states ps)
                           acc))
         '()
         lop))

; PuzzleState -> [List-of PuzzleState]
; generates all the possible next states moving from a single PuzzleState
(define (next-states ps)
  (local
    ((define sc (ps-scene ps))
     (define left (scene-left sc))
     (define right (scene-right sc))
     ; TODO: Could do something less repetive here
     ; or at least make a list of all-possible-scenes
     ; and then map the prevs onto them
     (define all-possible-moves
       (if (string=? (scene-boat sc) "left")
           ; BOAT ON LEFT SIDE
           (append
            ; have at least one of each
            (if (and (> (people-miss left) 0)
                     (> (people-cann left) 0))
                (list
                 (make-ps (make-scene (make-people (sub1 (people-miss left))
                                                   (sub1 (people-cann left)))
                                      (make-people (add1 (people-miss right))
                                                   (add1 (people-cann right)))
                                      "right")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least two Ms
            (if (> (people-miss left) 1)
                (list
                 (make-ps (make-scene (make-people (- (people-miss left) 2)
                                                   (people-cann left))
                                      (make-people (+ (people-miss right) 2)
                                                   (people-cann right))
                                      "right")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least two Cs
            (if (> (people-cann left) 1)
                (list
                 (make-ps (make-scene (make-people (people-miss left)
                                                   (- (people-cann left) 2))
                                      (make-people (people-miss right)
                                                   (+ (people-cann right) 2))
                                      "right")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least one M
            (if (> (people-miss left) 0)
                (list
                 (make-ps (make-scene (make-people (sub1 (people-miss left))
                                                   (people-cann left))
                                      (make-people (add1 (people-miss right))
                                                   (people-cann right))
                                      "right")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least one C
            (if (> (people-cann left) 0)
                (list
                 (make-ps (make-scene (make-people (people-miss left)
                                                   (sub1 (people-cann left)))
                                      (make-people (people-miss right)
                                                   (add1 (people-cann right)))
                                      "right")
                          (cons sc (ps-prev ps))))
                '())
            )
           ; BOAT ON RIGHT SIDE
           (append
            ; have at least one of each
            (if (and (> (people-miss right) 0)
                     (> (people-cann right) 0))
                (list
                 (make-ps (make-scene (make-people (add1 (people-miss left))
                                                   (add1 (people-cann left)))
                                      (make-people (sub1 (people-miss right))
                                                   (sub1 (people-cann right)))
                                      "left")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least two Ms
            (if (> (people-miss right) 1)
                (list
                 (make-ps (make-scene (make-people (+ (people-miss left) 2)
                                                   (people-cann left))
                                      (make-people (- (people-miss right) 2)
                                                   (people-cann right))
                                      "left")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least two Cs
            (if (> (people-cann right) 1)
                (list
                 (make-ps (make-scene (make-people (people-miss left)
                                                   (+ (people-cann left) 2))
                                      (make-people (people-miss right)
                                                   (- (people-cann right) 2))
                                      "left")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least one M
            (if (> (people-miss right) 0)
                (list
                 (make-ps (make-scene (make-people (add1 (people-miss left))
                                                   (people-cann left))
                                      (make-people (sub1 (people-miss right))
                                                   (people-cann right))
                                      "left")
                          (cons sc (ps-prev ps))))
                '())
            ; have at least one C
            (if (> (people-cann right) 0)
                (list
                 (make-ps (make-scene (make-people (people-miss left)
                                                   (add1 (people-cann left)))
                                      (make-people (people-miss right)
                                                   (sub1 (people-cann right)))
                                      "left")
                          (cons sc (ps-prev ps))))
                '())
            ))))
    (filter
     ; remove any repeated scenes
     (lambda (ps) (not (member (ps-scene ps) (ps-prev ps))))
     ; remove any scenes where the Ms die
     (filter ms-survive? all-possible-moves))))

; PuzzleState -> Boolean
(check-expect (ms-survive? initial) #t)
(check-expect (ms-survive? (make-ps (make-scene (make-people 2 3)
                                                (make-people 1 0)
                                                "left")
                                    '()))
                           #f)
(check-expect (ms-survive? (make-ps (make-scene (make-people 3 2)
                                                (make-people 0 1)
                                                "right")
                                    '()))
                           #t)
(check-expect (ms-survive? (make-ps (make-scene (make-people 2 1)
                                                (make-people 1 2)
                                                "left")
                                    '()))
                           #f)
(define (ms-survive? ps)
  (local
    ((define scene (ps-scene ps))
     (define left (scene-left scene))
     (define right (scene-right scene))
     ; People -> Boolean
     (define (side-safe p)
       (or
        (= (people-miss p) 0)
        (<= (people-cann p) (people-miss p)))))
    (and (side-safe left)
         (side-safe right))))
  

; PuzzleState -> PuzzleState
; is the final state reachable from state0
; generative creates a tree of possible boat rides 
; termination ???
;(check-expect (solve initial-puzzle) final-puzzle)
(define (solve state0)
  (local (; [List-of PuzzleState] -> PuzzleState
          ; generative generates the successors of los
          (define (solve* los)
            (cond
              [(ormap final? los)
               (first (filter final? los))]
              [else
               (solve* (create-next-states los))])))
    (solve* (list state0))))

; PuzzleState -> [List-of Scene]
; Produces a chronological list of scenes in a PuzzleState
(define (get-scenes ps)
   (reverse (cons (ps-scene solution)
                  (ps-prev solution))))
  

; PuzzleState -> Image
; Renders a PuzzlState
(define (render-ps ps)
  (local
    ((define all-scenes (cons (ps-scene ps)
                              (ps-prev ps))))
    (foldl (lambda (scene img) (above (render-scene scene) img))
           empty-image
           all-scenes)))

; Scene -> Image
; Renders a Scene in a PuzzleState 
(define (render-scene scene)
  (local
    ((define pRad 10)
     (define MISS (circle pRad "outline" "black"))
     (define CANN (circle pRad "solid" "black"))
     (define BOAT (rectangle (* pRad 2) pRad "solid" "brown"))
     ; People Image -> Image
     ; Renders a set of people
     (define (render-people p)
       (local
         ((define (render-line img n)
            (cond
              [(= 0 n) empty-image]
              [else (above img (render-line img (sub1 n)))]))) 
         (beside
          (render-line CANN (people-cann p))
          (render-line MISS (people-miss p)))))
     (define height (* 8 pRad))
     (define BANK (rectangle (* 6 pRad) height "outline" "black"))
     (define WATER (rectangle (* 10 pRad) height "solid" "blue"))
     (define BG (beside BANK WATER BANK)))
    (overlay/align/offset
     "left" "top"
     BOAT
     (if (string=? (scene-boat scene) "left")
         (- (image-width BANK))
         (- (- (image-width BG) (+ (image-width BOAT) (image-width BANK)))))
     (- (- (/ height 2) (/ pRad 2)))
     (overlay/align/offset
      "right" "top"
      (render-people (scene-right scene))
      (+ (/ pRad 2)) (- (/ pRad 2))
      (overlay/align/offset
       "left" "top"
       (render-people (scene-left scene))
       (- (/ pRad 2)) (- (/ pRad 2))
       BG)))))

(define solution (solve initial))
(define scenes-to-solution (get-scenes solution))

; To watch a video of the solution
; (run-movie 1 (map render-scene scenes-to-solution))


