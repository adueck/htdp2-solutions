;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-270) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nat -> [List-of Number]
; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (descend 0) '())
(check-expect (descend 3) '(0 -1 -2))
(define (descend n)
  (build-list n -))

; Nat -> [List-of Number]
; creates the list (list 1 ... n) for any natural number n
(check-expect (ascend 0) '())
(check-expect (ascend 3) '(1 2 3))
(define (ascend n)
  (build-list n add1))

; Nat -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (div-down 0) '())
(check-expect (div-down 3) '(1 1/2 1/3))
(define (div-down n)
  (local (; Nat -> Number
          (define (make-fact n)
            (/ 1 (add1 n))))
    (build-list n make-fact)))

; Nat -> [List-of Number]
; creates the list of the first n even numbers
(check-expect (first-even 4)
              '(0 2 4 6))
(define (first-even n)
  (local (; Nat -> Nat
          (define (doubleit n) (* n 2)))
    (build-list n doubleit)))

; BinaryNum is one of the following
; - 0
; - 1

; IMatrix is a [NEList-of IMatrixRow]

; IMatrixRow is a [NEList-of BinaryNum]

; Nat -> IMatrix
; creates an identity matrix square n wide
(check-expect (identityM.v2 1) (list (list 1)))
(check-expect (identityM.v2 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define (identityM.v2 n)
  (local
    ((define row-width n)
     ; Nat -> IMatrixRow
     ; Creates a row with a square at p
     (define (make-row p)
       (local
         (; Nat -> BinaryNum
          (define (place-sq q)
            (if (= p q) 1 0))) 
         (build-list row-width place-sq))))
    (build-list n make-row)))

; Number [Number -> Number] -> [List-of Number]
(check-expect (tabulate.v2 3 sqr)
              (list (sqr 3) (sqr 2) (sqr 1) (sqr 0)))
(define (tabulate.v2 n R)
  (reverse (build-list (add1 n) R)))
