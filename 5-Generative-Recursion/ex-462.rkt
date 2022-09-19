;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-462) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))
 
(define S '(1 1 2)) ; a Solution

; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))
 
; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; SOE Solution -> Boolean
; Determines if a given solution satifies all equations in a SOE
(check-expect (check-solution M S) #true)
(check-expect (check-solution M '(7 6 5)) #false)
(define (check-solution soe s)
  (andmap (lambda (equation)
            (= (plug-in (lhs equation) s)
               (rhs equation)))
          soe))

; [List-of Number] Solution -> Number
; Takes the left-hand side of an Equation and a Solution
; and calculates the value of the the left-hand side
(check-expect (plug-in '(2 2 3) '(1 1 2)) 10)
(define (plug-in leftS s)
  (if (not (= (length leftS) (length s)))
      (error "left hand and solution parameters unbalanced")
      (foldl (lambda (a b total) (+ (* a b) total))
             0
             leftS
             s)))