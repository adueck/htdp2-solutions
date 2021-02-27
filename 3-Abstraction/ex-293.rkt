;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-293) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(check-satisfied (find 3 '(1 3 4)) (found? 3 '(1 3 4)))
(check-satisfied (find 2 '(1 3 4)) (found? 2 '(1 3 4)))
(check-satisfied (find 4 '(1 3 4 5)) (found? 4 '(1 3 4 5)))
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))


; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
(define (found? x l)
  (lambda (l0)
    (cond
      [(boolean? l0) (andmap (lambda (i) (not (equal? x i))) l)]
      [else (and
             (equal? x (first l0))
             (is-end? l0 l))])))

; [X] [NE-List-of X] [NE-List-of X] -> Boolean
(check-expect (is-end? '(2 3) '(1 2 3)) #true)
(check-expect (is-end? '(4) '(2 6 8)) #false)
(check-expect (is-end? '(5) '(5)) #true)
(define (is-end? end parent)
  (cond
    [(empty? (rest end)) (equal? (first end) (last parent))]
    [else (and
           (equal? (last end) (last parent))
           (is-end? (w-out-last end) (w-out-last parent)))]))

; [List-of X] -> X
(check-expect (last '(1 2 3)) 3)
(define (last l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (last (rest l))]))

; [NE-List-of X] -> [NE-List-of X]
; returns a list without the last element on it
(check-expect (w-out-last '(1 2 3)) '(1 2))
(check-expect (w-out-last '(3 5 7 9)) '(3 5 7))
(define (w-out-last l)
  (reverse (rest (reverse l))))