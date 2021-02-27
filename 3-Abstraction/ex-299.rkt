;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-299) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Set is a function:
; [Number -> Boolean]
; interpretation returns whether an item is in a set or not

; Set Number -> Boolean
(check-expect (add-element odd-set 3) #true)
(check-expect (add-element odd-set 4) #false)
(define (add-element s n)
  (s n))

; Number -> Boolean
(check-expect (odd-set 3) #true)
(check-expect (odd-set 2) #false)
(define (odd-set ed)
  (odd? ed))

; Number -> Boolean
(check-expect (even-set 3) #false)
(check-expect (even-set 2) #true)
(define (even-set ed)
  (even? ed))

; Number -> Boolean
(check-expect (div-by-10 30) #true)
(check-expect (div-by-10 21) #false)
(define (div-by-10 ed)
  (zero? (modulo ed 10)))

; Set Set -> Set
; Makes a set which combines the elements of sets s1 and s2
(check-expect ((union div-by-10 odd-set) 3) #true)
(check-expect ((union div-by-10 odd-set) 10) #true)
(check-expect ((union div-by-10 odd-set) 4) #false)
(define (union s1 s2)
  (lambda (n)
    (or (s1 n) (s2 n))))

; Set Set -> Set
; Makes a set which collects the elements commond to sets s1 and s2
(check-expect ((intersect div-by-10 even-set) 20) #true)
(check-expect ((intersect div-by-10 even-set) 10) #true)
(check-expect ((intersect div-by-10 even-set) 6) #false)
(define (intersect s1 s2)
  (lambda (n)
    (and (s1 n) (s2 n))))

