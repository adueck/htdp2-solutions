;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-254) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] (Number Number -> Boolean) -> [List-of Number]
(check-expect (sort-n '(8 1 3 23) sf-n) '(1 3 8 23))
(check-expect (sort-n '() sf-n) '())
(define (sort-n l sf)
  (cond
    [(empty? l) '()]
    [else (put-in (first l) (sort-n (rest l) sf) sf)]))

; Number [List-of Number] (Number Number -> Boolean) -> [List-of Number]
(check-expect (put-in 3 '(1 4 6) sf-n) '(1 3 4 6))
(check-expect (put-in 4 '() sf-n) '(4))
(check-expect (put-in 10 '(0 4 8) sf-n) '(0 4 8 10))
(define (put-in n l sf)
  (cond
    [(empty? l) (cons n '())]
    [else (if
           (sf n (first l))
           (cons n l)
           (cons
            (first l)
            (put-in n (rest l) sf)))]))

; Number Number -> Boolean
(check-expect (sf-n 3 4) #t)
(check-expect (sf-n 4 1) #f)
(check-expect (sf-n 5 5) #t)
(define (sf-n n1 n2)
  (<= n1 n2))

; [List-of String] (String String -> Boolean) -> [List-of String]
(define (sort-s l sf)
  l)


; [List-of Number] (Number Number -> Boolean) -> [List-of Number]
;         |           |      |          |                |
; [List-of String] (String String -> Boolean) -> [List-of String]


; [List-of X] (X X -> Boolean) -> [List-of X]
(define (sort-n-abs l sf) l)

; [List-of X] (X X -> Boolean) -> [List-of X]
(define (sort-s-abs l sf) l)

; [List-of X] (X X -> Boolean) -> [List-of X]

; [List-of IR] (IR IR -> Boolean) -> [List-of IR]
(define (sort-ir l sf) l)