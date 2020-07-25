;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-145-sorted-temp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A CTemperature is a Number greater than -272.

; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures
(cons -273 '())
(cons 1 (cons 2 (cons 3 '())))

; NEList-of-temperatures -> Boolean
(check-expect (sorted>? (cons 1 '())) #true)
(check-expect (sorted>?
               (cons 1 (cons 2 (cons 3 '()))))
               #false)
(check-expect (sorted>?
               (cons 3 (cons 2 (cons 1 '()))))
               #true)
(check-expect (sorted>?
               (cons 3 (cons 3 (cons 1 '()))))
               #true)
(check-expect (sorted>?
               (cons 3 (cons 1 (cons 2 '()))))
               #false)
(define (sorted>? ne-lot)
  (cond
    [(empty? (rest ne-lot)) #true]
    [else (and
           (>= (first ne-lot) (first (rest ne-lot)))
           (sorted>? (rest ne-lot)))]))