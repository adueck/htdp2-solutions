;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-311) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct child [father mother name date eyes])
; A Child is a structure: 
;   (make-child Child Child String N String)

(define-struct no-parent [])

(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Boolean
; does an-ftree contain a child
; structure with "blue" in the eyes field
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #false]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

; FT -> Number
; counts the number of child structures in
; an-ftree
(check-expect (count-persons NP) 0)
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Gustav) 5)
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+
           1
           (count-persons (child-father an-ftree)) 
           (count-persons (child-mother an-ftree)))]))

; FT -> Number
; calculates the average age of all children in
; an-ftree given that the current year is y
(check-expect (average-age Carl 2000) 74)
(check-expect (average-age Gustav 2000) 229/5)
(define (average-age an-ftree y)
  (/
   (total-age an-ftree y)
   (count-persons an-ftree)))

; FT -> Number
; calculates the total age of all the children in
; an-ftree given that the current year is y
(check-expect (total-age Carl 2000) 74)
(check-expect (total-age Gustav 2000) 229)
(define (total-age an-ftree y)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+
           (- y (child-date an-ftree))
           (total-age (child-father an-ftree) y) 
           (total-age (child-mother an-ftree) y))]))

