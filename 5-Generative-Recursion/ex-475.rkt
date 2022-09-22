;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-475) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
        (list 'D (list 'D))
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
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
; It will find '(E C D), becauase that's the first one it finds when working
; through the neighbors in order
(check-expect (find-path 'C 'G sample-graph)
              #false)
(define (find-path orig dest g)
  (cond
    [(symbol=? orig dest) (list orig)]
    [else (local
            (; Node -> [List-of Node]
             (define (neighbors n)
               (second (assoc n g)))
             ; [List-of Node] -> [Maybe Path]
             (define (find-path-list origs)
               ; Would have been nice to have Racket's ormap here
               (findf
                list?
                (map (lambda (o) (find-path o dest g)) origs)))
             (define candidate (find-path-list (neighbors orig))))
            (if (false? candidate)
                #false
                (cons orig candidate)))]))

; X [X -> Boolean] -> [Maybe [List-of X]]
; Returns the first element of l that satisfies the predicate p?
; otherwise #false
(define (findf p? l)
  (cond
    [(empty? l) #false]
    [else (if (boolean=? #t (p? (first l)))
              (first l)
              (findf p? (rest l)))]))

; Node Node Graph -> [List-of Path]
(check-expect (find-all-paths 'E 'D sample-graph)
              '((E F D) (E C D)))
(check-expect (find-all-paths 'C 'G sample-graph) '())
(define (find-all-paths orig dest g)
  (cond
    [(symbol=? orig dest) (list (list orig))]
    [else (map
           (lambda (x) (cons orig x))
           (foldl
            (lambda (N coll) (append (find-all-paths N dest g) coll))
            '()
            (neighbors orig g)))]))

; Node Graph -> [List-of Node]
; Given a Node (n) in a Graph (g) returns the list of immediate neighbors
(check-expect (neighbors 'B sample-graph)
              (list 'E 'F))
(check-expect (neighbors 'D sample-graph) '())
(define (neighbors n g)
  (second (assoc n g)))

; Graph -> Boolean
; Determines if there is a path between all nodes on a graph
(check-expect (test-on-all-nodes sample-graph) #false)
(check-expect (test-on-all-nodes (list (list 'A (list 'B 'C))
                                       (list 'B (list 'C))
                                       (list 'C '())))
              #false)
(define (test-on-all-nodes g)
  ; Get
  ;  - all the nodes
  ;  - filter out the nodes that have no neighbors (end points)
  ;
  ; For each node that HAS NEIGHBORS - make pairs with every other possible node
  (local ((define all-nds (map (lambda (x) (first x)) g))
          (define all-nds-w-ns (filter
                                  (lambda (x) (cons? (neighbors x g))) all-nds))
          (define all-pairs (foldl
                             (lambda (a b) (append a b))
                             '()
                             (map (lambda (N) (make-pairs N all-nds)) all-nds-w-ns))))
  (andmap (lambda (pair) (not (false? (find-path (first pair) (second pair) g))))
          all-pairs)))

; Node [List-of Node]
(check-expect (make-pairs 'A (list 'A 'B 'C))
              (list (list 'A 'B)
                    (list 'A 'C)))
(check-expect (make-pairs 'A '()) '())
(define (make-pairs n all-nds)
  (map (lambda (x) (list n x))
         (remove n all-nds)))


