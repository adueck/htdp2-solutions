;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hanoi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define DISK-H 7)
(define DISK-W 75)
(define BASE-W (* (+ DISK-W 20) 3))
(define BASE-H (/ DISK-H 1.5))
(define PEG-H (+ (* DISK-H 6) (/ DISK-H 2)))
(define SCENE-H (+ PEG-H BASE-H))
(define SCENE-W BASE-W)
(define SCENE (empty-scene SCENE-W SCENE-H))

(define DISK-6 (rectangle DISK-W DISK-H "solid" "brown"))
(define DISK-5 (rectangle (- DISK-W 10) DISK-H "solid" "blue"))
(define DISK-4 (rectangle (- DISK-W 20) DISK-H "solid" "green"))
(define DISK-3 (rectangle (- DISK-W 30) DISK-H "solid" "yellow"))
(define DISK-2 (rectangle (- DISK-W 40) DISK-H "solid" "red"))
(define DISK-1 (rectangle (- DISK-W 50) DISK-H "solid" "pink"))
(define STACKED
  (list DISK-1 DISK-2 DISK-3 DISK-4 DISK-5 DISK-6))

(define BASE (rectangle BASE-W BASE-H "solid" "brown"))
(define PEG (rectangle (/ DISK-H 1.5) PEG-H "solid" "brown"))
(define PEG-A-X (* (/ BASE-W 5) 1))
(define PEG-B-X (/ BASE-W 2))
(define PEG-C-X (* (/ BASE-W 5) 4))
(define PEG-Y (- SCENE-H BASE-H))

(define EMPTY
  (place-image/align
   PEG
   PEG-C-X PEG-Y "center" "bottom"
  (place-image/align
   PEG
   PEG-B-X PEG-Y "center" "bottom"
  (place-image/align
   PEG
   PEG-A-X PEG-Y "center" "bottom"
   (place-image/align
   BASE
   0 SCENE-H "left" "bottom"
   SCENE)))))

; A PegName is one of
; - "a"
; - "b"
; - "c"

(define-struct gs [a b c])
; A GameState is a structure:
;   (make-gs [List-of Disk] [List-of Disk] [List-of Disk]). 
; interpretation shows the given disks stacked on pegs a, b, and c
(define gs1
  (make-gs STACKED '() '()))
(define gs2
  (make-gs
   (list DISK-1 DISK-2)
   (list DISK-3 DISK-4 DISK-5)
   (list DISK-6)))

; GameState -> Image
; Draws a given game state
(define (draw-gs gs)
  (draw-on-peg (gs-c gs) "c"
  (draw-on-peg (gs-b gs) "b"
  (draw-on-peg (gs-a gs) "a" EMPTY))))

; [List-of Disk] PegName Image -> Image
; Draws the scene with a given stack of disks on a peg identified by pn on a given scene, s 
(define (draw-on-peg lod pn s)
  (place-image/align (make-stack lod)
                     (cond
                       [(string=? pn "a") PEG-A-X]
                       [(string=? pn "b") PEG-B-X]
                       [(string=? pn "c") PEG-C-X])
                     PEG-Y "center" "bottom"
                     s))

; [List-of Disks] -> Image
; Makes an image of the disks stacked and centered on each other
(define (make-stack lod)
  (cond
    [(empty? lod) empty-image]
    [else (above (first lod) (make-stack (rest lod)))]))

; PegName PegName GameState -> GameState
; Transforms gs by moving a disk front the top of pile s to
; the top of pile d
(define (move-disk s d gs)
  (local ((define after-pick (pick-peg gs s)))
    (place-peg (first after-pick) d (second after-pick))))

; Disk PegName GameState -> GameState
; Places d on on given pn in a gs
(define (place-peg d pn gs)
  (cond
    [(string=? pn "a") (make-gs
                        (cons d (gs-a gs))
                        (gs-b gs)
                        (gs-c gs))]
    [(string=? pn "b") (make-gs
                        (gs-a gs)
                        (cons d (gs-b gs))
                        (gs-c gs))]
    [(string=? pn "c") (make-gs
                        (gs-a gs)
                        (gs-b gs)
                        (cons d (gs-c gs)))]))

; GameState PegName -> [Disk GameState]
; Picks the top disk out of a pile and returns the game state
(define (pick-peg gs pn)
  (cond
    [(string=? pn "a") (list
                        (first (gs-a gs))
                        (make-gs
                         (rest (gs-a gs))
                         (gs-b gs)
                         (gs-c gs)))]
    [(string=? pn "b") (list
                        (first (gs-b gs))
                        (make-gs
                         (gs-a gs)
                         (rest (gs-b gs))
                         (gs-c gs)))]
    [(string=? pn "c") (list
                        (first (gs-c gs))
                        (make-gs
                         (gs-a gs)
                         (gs-b gs)
                         (rest (gs-c gs))))]))
(define progress empty-image)

; a Move is a Tuple
; [PegName PegName]
; interpretation move the top item from PegName to PegName

; Number PegName PegName PegName [GameState [List-of Move]] -> [GameState [List-of Move]]
(define (move-tower n source dest spare gsm)
  (cond
    [(= n 1) (list
              (move-disk source dest (first gsm))
              (append (second gsm) (list (list source dest))))]
    [else
     (local ((define s1 (move-tower (sub1 n) source spare dest gsm))
             (define s2 (list
                         (move-disk source dest (first s1))
                         (append (second s1) (list (list source dest)))))
             (define s3 (move-tower (sub1 n) spare dest source s2)))
          s3)]))
 
(define (do-move gsm)
  (local ((define state (first gsm))
          (define moves (second gsm)))
    (cond
      [(empty? moves) (list state moves)]
      [else
       (list
        ; state
        (move-disk (first (first moves)) (second (first moves)) state)
        (rest moves))])))

; [GameState [List-of Move]]
(define (run-instructions ins)
  (big-bang ins
    [to-draw (lambda (x) (overlay/align
                          "left" "top"
                          (text (number->string (length (second x))) 20 "black")
                          (draw-gs (first x))))]
    [on-tick do-move 0.5]))

(define gs1moves (second (move-tower 6 "a" "b" "c" (list gs1 '()))))
(run-instructions (list gs1 gs1moves))