;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-471) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Node is a Symbol.

; A Graph is a [List-of (list Node [List-of Symbol]]
; Interpretation each list represents a node
;  the first item of the list is the name of the node
;  the second item is a list containing all the neighbors, ie.
;  paths from the node. (Empty if none)
(define sample-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'D))
        (list 'D '())
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))

; Node Graph -> [List-of Node]
; Given a Node (n) in a Graph (g) returns the list of immediate neighbors
(check-expect (neighbors 'B sample-graph)
              (list 'E 'F))
(check-expect (neighbors 'D sample-graph) '())
(define (neighbors n g)
  (second (assoc n g)))