;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-164-converteuro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define RATE 1.14)

; Number -> Number
; converts n in Euro to US$
(check-expect (eutous 3) (* 3 RATE))
(define (eutous n)
  (* n RATE))

; List-of-numbers -> List-of-numbers
; converts o list of Euro amounts to US$ Amounts
(check-expect (convert-euro '()) '())
(check-expect (convert-euro
               (cons 3 '()))
              (cons (eutous 3) '()))
(define (convert-euro lon)
  (cond
    [(empty? lon) '()]
    [else (cons (eutous (first lon)) (rest lon))]))

; List-of-numbers Number -> List-of-numbers
; converts o list of Euro amounts to US$ Amounts
; according to r rate
(check-expect (convert-euro* '() 2) '())
(check-expect (convert-euro*
               (cons 3 '()) 2)
              (cons 6 '()))
(define (convert-euro* lon r)
  (cond
    [(empty? lon) '()]
    [else (cons (* (first lon) r) (rest lon))]))