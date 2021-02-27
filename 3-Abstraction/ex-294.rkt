;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-294) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(check-expect (index 2 '(1 2 3)) 1)
(check-expect (index 5 '(1 2 3)) #false)
(check-satisfied (index 2 '(1 2 3)) (is-index? 2 '(1 2 3)))
(check-satisfied (index 5 '(1 2 3)) (is-index? 5 '(1 2 3)))
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))

; X [List-of X] -> [[Maybe N] -> Boolean]]
(check-expect [(is-index? 3 '(1 2 3)) (index 3 '(1 2 3))] #true)
(define (is-index? x l)
  (lambda (n)
    (cond
      [(boolean? n)
       (andmap (lambda (j) (not (equal? j x))) l)]
      [else
       (x-is-first-at x n l)])))

; X Number [List-of X] -> Boolean
(check-expect (x-is-first-at 2 1 '(1 2 3)) #true)
(check-expect (x-is-first-at 2 2 '(1 2 3)) #false)
(check-expect (x-is-first-at 2 1 '(2 2 3)) #false)
(define (x-is-first-at x n l)
  (cond
    [(zero? n) (equal? x (first l))]
    [else (and
           (not (equal? x (first l)))
           (x-is-first-at x (sub1 n) (rest l)))]))