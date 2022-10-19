;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-507) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [X Y] [X Y -> Y] Y [List-of X] -> Y
(check-expect (f*ldl + 0 '(1 2 3))
              (foldl + 0 '(1 2 3)))
(check-expect (f*ldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(define (f*ldl f e l0)
  (local (; [X Y] Y [List-of X] -> Y
          (define (fold/a a l)
            (cond
              [(empty? l) a]
              [else
               (fold/a (f (first l) a) (rest l))])))
    (fold/a e l0)))

; [X] Number [Number -> X] -> [List-of X]
; Creates a list of n elements by applying proc
; to the integers from 0 to (sub1 n) in order.
(check-expect (build-l*st 3 add1) (build-list 3 add1))
(check-expect (build-l*st 10 sub1) (build-list 10 sub1))
(define (build-l*st n0 proc)
  (local (; Number [List-of X]
          ; accumulator a is a list of the numbers between
          ; n0 and n that have been processed
          (define (bl/a n a)
            (cond
              [(= n n0) (reverse a)]
              [else (bl/a (add1 n) (cons (proc n) a))])))
    (bl/a 0 '())))