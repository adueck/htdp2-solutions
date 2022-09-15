;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-447) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Number -> Number
; roots 2 and 4
(define (poly1 x)
  (* (- x 2) (- x 4)))
; root 0
(define (poly2 x)
  (* x x))
; root -1
(define (poly3 x)
  (+ (* x x x) 1))


; tolerance for error
(define ε 0.01)

(define (withinE n m)
  (<= (abs (- n m)) ε))


; [Number -> Number] Number Number -> Number
; determines R such that f has a root in [R,(+ R ε)]
; assume f is continuous 
; (2) (or (<= (f left) 0 (f right)) (<= (f right) 0 (f left)))
; generative divides interval in half, the root is in 
; one of the two halves, picks according to (2)
(check-satisfied (find-root poly1 2.5 30) (lambda (a) (withinE a 4)))
(check-satisfied (find-root poly2 -20 20) (lambda (a) (withinE a 0)))
(check-satisfied (find-root poly3 -20 20) (lambda (a) (withinE a -1)))
(define (find-root f left right)
  (cond
    [(withinE left right) left]
    [else
      (local ((define mid (/ (+ left right) 2))
              (define f@mid (f mid)))
        (cond
          [(or (<= (f left) 0 f@mid) (<= f@mid 0 (f left)))
           (find-root f left mid)]
          [(or (<= f@mid 0 (f right)) (<= (f right) 0 f@mid))
           (find-root f mid right)]))]))

; This returns "all question results were false" error because
; The algorithim only looks for the root on the right side
(find-root poly1 -20 20)