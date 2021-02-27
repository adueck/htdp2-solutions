;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-285) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of USD] -> [List-of EUR]
(check-expect (convert-euro '()) '())
(check-expect (convert-euro (list 10 12))
              (list (/ 10 1.06) (/ 12 1.06)))
(define (convert-euro lou)
  (map (lambda (u) (/ u 1.06)) lou))

; [List-of F] -> [List-of C]
(check-expect (convertFC '()) '())
(check-expect (convertFC (list 20 -40))
              (list (* (- 20 32) (/ 5 9))
                    (* (- -40 32) (/ 5 9))))
(define (convertFC lof)
  (map (lambda (x) (* (- x 32) (/ 5 9))) lof))

; [List-of posn] -> [List-of [number, number]]
(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 2 3) (make-posn 10 2)))
                         (list (list 2 3) (list 10 2)))
(define (translate lop)
  (map (lambda (x)
         (list (posn-x x) (posn-y x)))
       lop))