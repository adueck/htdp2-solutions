;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |482|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require "extras.rkt")

(define SIZE 50)
(define SQ (square SIZE "outline" "black"))
(define Q (circle (* 0.25 SIZE) "outline" "black"))
(define Q-WIDTH (image-width Q))
(define QUEENS 8)

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

; QP Image -> Image
(define (place-on p img)
  (local ((define (offset x) (-
                              (- (/ Q-WIDTH 2) (/ SIZE 2))
                              (* SIZE x)))
          (define x-pos (offset (posn-x p)))
          (define y-pos (offset (posn-y p))))
      (add-lines
       p
       (overlay/align/offset
        "left" "top" Q
        x-pos
        y-pos
        img))))

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

; [List-of QP] Number -> Image
(define (render-queens lop n)
  (foldl (lambda (p img) (place-on p img))
         (draw-board n)
         lop))

; N -> [Maybe [List-of QP]]
; finds a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
(define 4QUEEN-NOT-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 3 3) (make-posn 3 1)))

;(check-expect (n-queens 3) #false)
;(check-expect (n-queens 2) #false)
;(check-satisfied (n-queens 5) (n-queens-solution? 5))
(define (n-queens n)
  (place-queens (board0 n) n))

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

(define (board0 n) '())