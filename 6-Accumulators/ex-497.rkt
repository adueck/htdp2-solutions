;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-497) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

; Tree -> Number
; calculates the height (levels) of a given binary tree
(check-expect (height example) 3)
(define (height abt)
  (cond
    [(empty? abt) 0]
    [else (+ (max (height (node-left abt))
                  (height (node-right abt))) 1)]))

; Tree -> Number
; calculates the height (levels) of a given binary tree
(check-expect (height.v2 example) 3)
(define (height.v2 abt0)
  (local (; Tree ??? -> N
          ; measures the height of abt
          ; accumulator a is the number of steps
          ; it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
                (max (height/a (node-left abt) (add1 a))
                     (height/a (node-right abt) (add1 a)))])))
    (height/a abt0 0)))