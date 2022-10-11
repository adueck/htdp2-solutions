;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-492) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

(define cyclic-graph
  (list (list 'A (list 'B 'E))
        (list 'B (list 'E 'F))
        (list 'C (list 'B 'D))
        (list 'D '())
        (list 'E (list 'C 'F))
        (list 'F (list 'D 'G))
        (list 'G '())))


; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 
 
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false

; Node Node Graph [List-of Node] -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
(check-expect (find-path 'C 'D sample-graph '())
              '(C D))
(check-member-of (find-path 'E 'D sample-graph '())
                 '(E F D) '(E C D))
(check-member-of (find-path 'B 'D cyclic-graph '())
                 '(B E C D) '(B E F D) '(B F D))
(define (find-path origination destination G seen)
  (cond
    [(symbol=? origination destination) (list destination)]
    ; If we're working from a point that's already been seen,
    ; the path is cyclical and therefore doesn't work
    [(member? origination seen) #false]
    [else (local ((define next (neighbors origination G))
                  (define candidate
                    (find-path/list next destination G (cons origination seen))))
            (cond
              [(boolean? candidate) #false]
              [else (cons origination candidate)]))]))
 
; [List-of Node] Node Graph [List-of Node] -> [Maybe Path]
; finds a path from some node on lo-Os to D
; if there is no path, the function produces #false
(define (find-path/list lo-Os D G seen)
  (cond
    [(empty? lo-Os) #false]
    [else (local ((define candidate
                    (find-path (first lo-Os) D G seen)))
            (cond
              [(boolean? candidate)
               (find-path/list (rest lo-Os) D G seen)]
              [else candidate]))]))

; NOTE: This could be neatened up, put in one function and then wrapped
; in a function without an accumulator for starting it, but I get the idea
; and did this in TypeScript https://stackblitz.com/edit/react-ts-pudkgw?file=App.tsx
; so moving on

; Node Graph -> [List-of Node]
; Given a Node (n) in a Graph (g) returns the list of immediate neighbors
(check-expect (neighbors 'B sample-graph)
              (list 'E 'F))
(check-expect (neighbors 'D sample-graph) '())
(define (neighbors n g)
  (second (assoc n g)))
