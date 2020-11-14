;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-267-map-exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; USAmount is a Number
; representing the amount of US$

; EUAmount is a Number
; represeting the amount of EU€

; [List-of USAmount] -> [List-of EUAmount]
; converts a list of US$ amounts into a list of € amounts
; based on an exchange rate of US$1.06 per €
(check-expect (convert-euro (list 1 2.04))
              (list (/ 1 1.06) (/ 2.04 1.06)))
(check-expect (convert-euro '()) '())
(define (convert-euro lou)
  (local (; USAmount -> EUAmount
          ; converts a USD amount to a EUR amount
          (define (usd-to-eur us)
            (/ us 1.06)))
    (map usd-to-eur lou)))

; FTemp is a Number
; representing degrees in Fahrenheit

; CTemp is a Number
; representing degrees in Celsius

; [List-of FTemp] -> [List-of CTemp]
; converts a list of Fahrenheit measurements
; to a list of Celsius measurements
(check-expect (convertFC '()) '())
(check-expect (convertFC (list 32 86))
              (list 0 30))
(define (convertFC lof)
  (local (; FTemp -> CTemp
          ; converts a temperature from Fahrenheit to Celsius
          (define (f-to-c ft)
            (* (- ft 32) (/ 5 9))))
    (map f-to-c lof)))

; PairOfNums is a List
; (list Num Num)

; [List-of Posn] -> [List-of PairOfNums]
; translates a list of Posns into a
; list of lists of pairs of numbers
(check-expect (translate '()) '())
(check-expect (translate (list
                          (make-posn 2 4)
                          (make-posn 5 9)))
              (list
               (list 2 4)
               (list 5 9)))
(define (translate lop)
  (local (; Posn -> PairOfNums
          ; converts a Posn into a PairOfNums
          (define (posn-to-pair p)
            (list (posn-x p) (posn-y p))))
    (map posn-to-pair lop)))