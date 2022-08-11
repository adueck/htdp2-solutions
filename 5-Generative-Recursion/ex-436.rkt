;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-436) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define FIELD-SIZE 40)

; Snake -> Food 
; Creates a new random position for the food which
; was not the previous p and also does not cover the Snake (s)
; termination the function loops if the snake is covery evy position in
; the field
(check-satisfied (food-create (list (make-posn 1 1))) not=-1-1?)
(define (food-create s)
  (if (snake-covers-whole-field s)
      (error "can't create food because the snake is covering the whole field")
      (local
        ((define (food-check-create candidate)
           (if (member? candidate s)
               (food-create s)
               candidate)))
        (food-check-create
         (make-posn (random FIELD-SIZE) (random FIELD-SIZE))))))

; Snake -> Boolean
; Checks to see if a snake covers the entire playing field
(define (snake-covers-whole-field s)
  (local
    ((define (snake-covers-rows r s)
       (cond
         [(= r 0) (covers-row s r FIELD-SIZE)]
         [else (and
                (covers-row s r FIELD-SIZE)
                (snake-covers-rows s (sub1 r)))])))
    (snake-covers-rows FIELD-SIZE s)))

; Snake Number Number -> Boolean
; Checks to see if a snake covers a full row up to column c
(check-expect (covers-row
               (list (make-posn 3 0) (make-posn 3 1) (make-posn 3 2))
               3 2)
              #t)
(check-expect (covers-row
               (list (make-posn 3 0) (make-posn 2 1) (make-posn 3 2))
               3 2)
              #f)
(define (covers-row s r c)
  (cond
    [(= c 0) (member? (make-posn r c) s)]
    [else (and (member? (make-posn r c) s)
               (covers-row s r (sub1 c)))]))


; The trivially solvable problem is creating a random position
; it needs to be combined with checking if the random number is not overlapping
; with a previous position, and if so then we recursively create another position and
; check it

; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
