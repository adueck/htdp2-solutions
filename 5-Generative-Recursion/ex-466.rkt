;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-466) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; [List-of X] Number -> [List-of X]
(check-expect (list-tail '(1 2 3 4 5) 2) '(3 4 5))
(check-expect (list-tail '(1 2) 0) '(1 2))
(define (list-tail l n)
  (cond
    [(empty? l) '()]
    [(>= 0 n) l]
    [else (list-tail (rest l) (sub1 n))]))

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
(check-expect (plug-in '(3 9) '(1 1 2)) 21)
(check-expect (plug-in '(1) '(1 1 2)) 2)
(define (plug-in leftS s)
  (foldl (lambda (a b total) (+ (* a b) total))
         0
         leftS
         (list-tail s (- (length s) (length leftS)))))

; Equation Equation -> Equation
; Subtracts E1 from E2 so that the leading coefficient will be zero,
; and therefore eliminated
(check-expect (subtract '(2 2 3 10) '(2 5 12 31)) '(3 9 21))
(check-expect (subtract '(2 2 3 10) '(4 1 -2 1)) '(-3 -8 -19))
(define (subtract E1 E2)
  (local ((define subtract-times (/ (first E2) (first E1)))
          (define subtracted (map (lambda (a b) (- b (* subtract-times a)))
                                  E1 E2)))
  (rest subtracted)))

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate M)
              (list (list 2 2 3 10)
                    (list 3 9 21)
                    (list 1 2)))
(define (triangulate M)
  (if (<= (length (first M)) 2)
      M
      (local ((define topE (first M))
              (define lowerEs (rest M))
              (define subtracted (map
                                  (lambda (x) (subtract topE x))
                                  lowerEs)))
        (cons topE (triangulate subtracted)))))

; 1. What is the trivially solvable problem?
;  - subtract from the first equation to eliminate the coefficient
; 2. How are trivial problems solved?
;  - by using subtract, and subtracting away the leading co-efficient
;    from all of the equations below the first one
; 3. How does the algorithm generate new problems that are more easily
; solvable than the original one? Is there one new problem that we generate
; or several?
;  - Now we have the same situation, but with n-1 equations left that we need to
;    subtract leading co-efficients from.
; 4. Is the solution of the given problem the same as the solution of (one of)
; the new problems? Or, do we need to combine the solutions to create a solution
; for the original problem? And, if so, do we need anything from the original
; problem data?
;  - We just need to keep travesing down and subtracting the rows below as we
;    did in the original problem