;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-144-average-temp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A CTemperature is a Number greater than -272.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures
(cons -273 '())
(cons 1 (cons 2 (cons 3 '())))

; NEList-of-temperatures -> Number
; computes the average temperature 
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))
 
; List-of-temperatures -> Number 
; adds up the temperatures on the given list
(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)
(define (sum alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot) (+
                   (first alot)
                   (sum (rest alot)))]))
 
; List-of-temperatures -> Number 
; counts the temperatures on the given list
(check-expect
  (how-many (cons 1 (cons 2 (cons 3 '())))) 3)
(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot) (add1 (how-many (rest alot)))]))

; List-of-temperatures -> Number
; computes the average temperature
(check-expect
  (checked-average (cons 1 (cons 2 (cons 3 '())))) 2)
(check-error
  (checked-average '()) "cannot use empty list")
(define (checked-average alot)
  (cond
    [(cons? alot) (average alot)]
    [(empty? alot) (error "cannot use empty list")]))

; Yes, we see that sum and how-many work for NEList-of-temperatures
; because they were designed to work with List-of-temperatures which
; is a wider definition that also includes all the possible data for
; NEList-of-temperatuers