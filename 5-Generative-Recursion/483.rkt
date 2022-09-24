;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |483|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require "extras.rkt")

(define SIZE 40)
(define SQ (square SIZE "outline" "black"))
(define Q (circle (* 0.25 SIZE) "solid" "grey"))
(define Q-WIDTH (image-width Q))
(define QUEENS 8)

; Number Boolean -> Image
; Draws a picture of a solution to the queens problem
;  given a number of n and a boolean to show the threatening lines or not
(define (render-solution n show-lines)
  (local ((define solution (n-queens n)))
    (if (boolean? solution)
        (overlay
         (text "No Solution" 24 "red")
         (draw-board n))
        (render-board
          (make-board n solution)
          show-lines))))

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
(define 4QUEEN-NOT-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 3 3) (make-posn 3 1)))

(check-expect (n-queens 3) #false)
(check-expect (n-queens 2) #false)
(check-satisfied (n-queens 5) (n-queens-solution? 5))
(define (n-queens n)
  (local
    ((define candidate (place-queens (board0 n) n)))
    (if (boolean? candidate)
        #false
        (board-qps candidate))))

; Number -> [[List-of QP] -> Boolean]
; Returns a predicate on queen placements that determines
; whether a given placement is a solution to the queens puzzle
(check-satisfied 4QUEEN-SOLUTION-2 (n-queens-solution? 4))
(check-satisfied 4QUEEN-NOT-SOLUTION-2 (lambda (s) (not ((n-queens-solution? 4) s))))
(define (n-queens-solution? n)
  (local ((define (threatening-others? pq solution)
            (ormap (lambda (sol-pq)
                     (if (and (= (posn-x pq) (posn-x sol-pq))
                              (= (posn-y pq) (posn-y sol-pq)))
                         #false
                         (threatening? pq sol-pq)))
                   solution)))
    (lambda (solution)
      (and (= (length solution) n)
           (not (ormap
                 (lambda (pq) (threatening-others? pq solution))
                 solution))))))

; Board Boolean -> Image
(define (render-board b show-lines)
  (render-queens (board-qps b) (board-size b) show-lines))

; [List-of QP] Number Boolean -> Image
(define (render-queens lop n show-lines)
  (foldl (lambda (p img) (place-on p img show-lines))
         (draw-board n)
         lop))

; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column
(check-expect (threatening? (make-posn 0 0)
                           (make-posn 1 0))
              #true)
(check-expect (threatening? (make-posn 2 2)
                           (make-posn 1 1))
              #true)
(check-expect (threatening? (make-posn 1 1)
                           (make-posn 0 2))
              #true)
(check-expect (threatening? (make-posn 0 0)
                           (make-posn 2 1))
              #false)
(define (threatening? qp1 qp2)
  (local ((define (hits-x)
            (= (posn-x qp1) (posn-x qp2)))
          (define (hits-y)
            (= (posn-y qp1) (posn-y qp2)))
          (define (hits-diag-up)
            ; slope = 1
            (= (- (posn-y qp2) (posn-y qp1))
               (- (posn-x qp2) (posn-x qp1))))
          (define (hits-diag-down)
            ; slope = -1
            (=
             (- (posn-y qp2) (posn-y qp1))
             (- (- (posn-x qp2) (posn-x qp1))))))
  (or (hits-x)
      (hits-y)
      (hits-diag-up)
      (hits-diag-down))))

; Number -> Image
(define (draw-board n)
  (local ((define ROW (draw-row n))
          (define (drawB m)
            (cond
              [(= m 0) empty-image]
              [else (above ROW (drawB (sub1 m)))])))
    (drawB n)))

; Number -> Image
(define (draw-row n)
  (cond
    [(= n 0) empty-image]
    [else (beside SQ (draw-row (sub1 n)))]))

; QP Image Boolean -> Image
(define (place-on p img show-lines)
  (local ((define (offset x) (-
                              (- (/ Q-WIDTH 2) (/ SIZE 2))
                              (* SIZE x)))
          (define x-pos (offset (posn-x p)))
          (define y-pos (offset (posn-y p)))
          (define placed (overlay/align/offset
                          "left" "top" Q
                          x-pos
                          y-pos
                          img)))
    (if show-lines
        (add-lines p placed)
        placed)))

; QP Image -> Image
; Adds the threatening lines for a given QP on a board
(define (add-lines p img)
  (local ((define y-mid (+ (* (posn-y p) SIZE) (/ SIZE 2)))
          (define x-mid (+ (* (posn-x p) SIZE) (/ SIZE 2)))
          (define diag-off (* SIZE 0.2)))
    ; Threatening Down-Left
    (scene+line
     ; Threatening Down-Right
     (scene+line
      ; Threatening Up-Left
      (scene+line
       ; Threatening Up-Right
       (scene+line
        ; Threatening Down
        (add-line
         ; Threatening Up
         (add-line
          ; Threatening Left
          (add-line
           ; Threatening Right
           (add-line
            img         
            (+ (* (posn-x p) SIZE) (* SIZE 0.75))
            y-mid
            (image-width img)
            y-mid
            "red")
           (+ (* (posn-x p) SIZE) (* SIZE 0.25))
           y-mid
           0
           y-mid
           "red")
          x-mid
          0 
          x-mid
          (+ (* (posn-y p) SIZE) (* SIZE 0.25))
          "red")
         x-mid
         (+ (* (posn-y p) SIZE) (* SIZE 0.75))
         x-mid
         (image-height img)
         "red")
        (+ x-mid diag-off)
        (- y-mid diag-off)
        (+ x-mid y-mid)
        0
        "red"
        )
       (- x-mid diag-off)
       (- y-mid diag-off)
       (- x-mid y-mid)
       0
       "red"
       )
      (+ x-mid diag-off)
      (+ y-mid diag-off)
      (+ x-mid (- (image-height img) y-mid))
      (image-height img)
      "red"
      )
     (- x-mid diag-off)
     (+ y-mid diag-off)
     (- x-mid (- (image-height img) y-mid))
     (image-height img)
     "red"
     )))

; [List-of X] [List-of X] -> Boolean
; Determines if two lists contain the same items
; (Done quickly. A more correct/thorough approach would be to make the lists unique first)
(check-expect (set=? (list 3 1 2) (list 1 2 3)) #true)
(check-expect (set=? '() '()) #true)
(check-expect (set=? (list 2 2 1) (list 2 1 3)) #false)
(check-expect (set=? (list 1 2) (list 1 2 3)) #false)
(define (set=? a b)
  (cond
    [(not (= (length a) (length b))) #false]
    [else (and
           (andmap (lambda (x) (member x b)) a)
           (andmap (lambda (x) (member x a)) b))]))

(define-struct board [size qps])
; A Board is a structure:
;  (make-board Number [List-of QP])

; Board N -> [Maybe [List-of QP]]
; places n queens on board; otherwise, returns #false
(define (place-queens a-board n)
  (cond
    [(= n 0) a-board]
    [else (ormap (lambda (open-spot)
                   (place-queens 
                    (add-queen a-board open-spot)
                    (sub1 n)))
                 (find-open-spots a-board))]))

; N -> Board 
; creates the initial n by n board
(check-expect (board0 5) (make-board 5 '()))
(check-expect (board0 10) (make-board 10 '()))
(define (board0 n)
  (make-board n '()))

; Board QP -> Board 
; places a queen at qp on a-board
(check-expect (add-queen (board0 3) (make-posn 2 3))
              (make-board 3 (list (make-posn 2 3))))
(define (add-queen a-board qp)
  (make-board
   (board-size a-board)
   (cons qp (board-qps a-board))))

; Board -> [List-of QP]
; finds spots where it is still safe to place a queen
(check-expect (set=? (find-open-spots (board0 2))
                     (list (make-posn 0 0) (make-posn 0 1)
                           (make-posn 1 0) (make-posn 1 1)))
              #true)
(check-expect (set=? (find-open-spots (make-board 3 (list (make-posn 0 0))))
                     (list (make-posn 2 1) (make-posn 1 2)))
              #true)
(check-expect (find-open-spots (make-board 2 (list (make-posn 0 0))))
              '())
(define (find-open-spots a-board)
  (filter (lambda (spot)
            (andmap (lambda (x) (not (threatening? x spot))) (board-qps a-board)))
          (all-spots a-board)))

; Board -> [List-of QP]
(define (all-spots b)
  (local ((define ps (cons 0 (build-list (sub1 (board-size b)) add1))))
    (foldl
     (lambda (y l) (append
                    (map (lambda (x) (make-posn x y)) ps)
                    l))
     '()
     ps)))

 