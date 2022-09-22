;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-470) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
(check-expect (triangulate
               (list (list 2 3 3 8)
                     (list 2 3 -2 3)
                     (list 4 -2 2 4)))
              (list (list 2 3 3 8)
                    (list -8 -4 -12)
                    (list -5 -5)))
(check-error (triangulate
              (list (list 0 0 2 6)
                    (list 0 0 1 0))))
(define (triangulate M)
  (cond
    [(andmap (lambda (E) (= (first E) 0)) M) (error "all leading coefficients are zero")]
    [(<= (length (first M)) 2) M]
    [else (local ((define safeM (avoid-top-zero M))
                  (define topE (first safeM))
                  (define lowerEs (rest safeM))
                  (define subtracted
                    (map (lambda (x) (subtract topE x)) lowerEs)))
        (cons topE (triangulate subtracted)))]))

; SOE -> SOE
; Rotates a system of equations as necessary to avoid the first one starting with a 0
(check-expect (avoid-top-zero (list
                               (list 1 2 3)
                               (list 2 4 5)))
                              (list (list 1 2 3)
                                    (list 2 4 5)))
(check-expect (avoid-top-zero (list
                               (list 0 2 3)
                               (list 2 4 5)))
                              (list (list 2 4 5)
                                    (list 0 2 3)))
(define (avoid-top-zero M)
  (if (not (= 0 (first (first M))))
      M
      (avoid-top-zero (rotate M))))

; [List-of X] -> [List-of X]
; Rotates a non-empty list to start with the next item
(check-expect (rotate (list 1 2 3)) (list 2 3 1))
(define (rotate L)
  (append (rest L) (list (first L))))

; SOE -> Solution
; Shows the solution for a given triangulated SOE
; Using the existing foldr abstraction and lambda, as per the Challenge
(check-expect (solve (triangulate M)) S)
(check-expect (solve (list (list 2 4)))
              (list 2))
(define (solve soe)
  (foldr (lambda (e los) (cons (solve-helper e los) los))
         '()
         soe))

; SOE -> Solution
; Shows the solution for a given SOE using the triangulation method
(check-expect (gauss M) S)
(check-expect (gauss (list (list 2 4)))
              (list 2))
(define (gauss soe)
  (solve (triangulate soe)))

; Equation [List-of Number] -> Number
; Takes an equation with solutions for the non-leading coefficients
; and finds the solution for the leading coefficient
(check-expect (solve-helper (list 2 2 3 10) (list 1 2))
              1)
(check-expect (solve-helper (list 2 4) '()) 2)
(define (solve-helper e lon)
  (if (= 2 (length e))
      (/ (second e) (first e))
      (local ((define lSide (lhs e))
              (define rSide (rhs e))
              (define leadingCo (first lSide))
              (define otherCos (rest lSide))
              (define otherCosVal (plug-in otherCos lon)))
        (/ (- rSide otherCosVal) leadingCo))))
