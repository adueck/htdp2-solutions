;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex-253) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Number -> Boolean]
; Number -> Boolean
; checks if a given number is even
(check-expect (is-even? 5) #f)
(check-expect (is-even? 2) #t)
(define (is-even? n)
  (if (= (modulo n 2) 0)
      #true
      #false))

; [Boolean String -> Boolean]
; Tells whether a word is
;  - big (more than 3 chars) when o is #false
;  - huge (more than 6 chars) when o is #true
(check-expect (big-word? #false "car") #false)
(check-expect (big-word? #false "cars") #true)
(check-expect (big-word? #true "cars") #false)
(check-expect (big-word? #true "fantastic") #true)
(define (big-word? o s)
  (> (string-length s) (cond
                         [(boolean=? o #f) 3]
                         [(boolean=? o #t) 6])))

; [Number Number Number -> Number]
; returns the sum of 3 numbers
(check-expect (triplet-sum 2 4 6) 12)
(check-expect (triplet-sum 2 -2 1) 1)
(define (triplet-sum n1 n2 n3)
  (+ n1 n2 n3))

; [Number -> [List-of Number]]
; slices a number up into a list of single 1s
(check-expect (slice-number 0) '())
(check-expect (slice-number 0.3) '())
(check-expect (slice-number 4) '(1 1 1 1))
(define (slice-number n)
  (cond
    [(< n 1) '()]
    [else
     (cons 1 (slice-number (sub1 n)))]))

; [[List-of Number] -> Boolean]
; checks if the sum of all numbers in a given list is even
(check-expect (total-is-even? (list 3 1 1)) #f)
(check-expect (total-is-even? (list 3 1 2)) #t)
(define (total-is-even? lon)
  (is-even? (total lon)))

; returns the sum of a list of numbers
(check-expect (total (list 3 1 5)) 9)
(check-expect (total (list 0 0)) 0)
(check-expect (total '()) 0)
(define (total lon)
  (cond
    [(empty? lon) 0]
    [else (+
           (first lon)
           (total (rest lon)))]))