;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-288) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (q1 3) (list 0 1 2))
(check-expect (q1 6) (list 0 1 2 3 4 5))
; Number -> [List-of Number]
(define (q1 n)
  (build-list n (lambda (x) x)))

(check-expect (q2 3) (list 1 2 3))
(check-expect (q2 6) (list 1 2 3 4 5 6))
; Number -> [List-of Number]
(define (q2 n)
  (build-list n (lambda (x) (+ x 1))))

(check-expect (q3 3) (list 1 (/ 1 2) (/ 1 3)))
(check-expect (q3 6) (list 1 (/ 1 2) (/ 1 3) (/ 1 4) (/ 1 5) (/ 1 6)))
; Number -> [List-of Number]
(define (q3 n)
  (build-list n (lambda (x) (/ 1 (+ x 1)))))

(check-expect (q4 5) (list 2 4 6 8 10))
(check-expect (q4 3) (list 2 4 6))
; Number -> [List-of Number]
(define (q4 n)
  (build-list n (lambda (x) (* (+ x 1) 2))))

(check-expect (make-row 0 3) (list 1 0 0))
(check-expect (make-row 2 5) (list 0 0 1 0 0))
(define (make-row pos w)
  (append (make-list pos 0) (list 1) (make-list (- (sub1 w) pos) 0)))

(check-expect (q5 1) (list (list 1)))
(check-expect (q5 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
; Number -> [List-of [List-of Number]]
(define (q5 n)
  (build-list
   n
   (lambda (x) (make-row x n))))

(check-expect (tabulate add1 3) (list 1 2 3))
(check-expect (tabulate sqr 4) (list 0 1 4 9))
; Number -> [List-of Number]
(define (tabulate f n)
  (build-list
   n
   (lambda (x) (f x))))