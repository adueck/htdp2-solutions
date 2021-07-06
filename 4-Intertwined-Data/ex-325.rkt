;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-325) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define tree1 (make-node
  15
  'd
  NONE
  (make-node
    24 'i NONE NONE)))
(define tree2 	
(make-node
  15
  'd
  (make-node
    87 'h NONE NONE)
  NONE))

; A BST (short for binary search tree) is a BT according to the following conditions:
; NONE is always a BST.

; (make-node ssn0 name0 L R) is a BST if:
; - L is a BST,
; - R is a BST,
; - all ssn fields in L are smaller than ssn0,
; - all ssn fields in R are larger than ssn0.

(define bst1 (make-node
              63
              'a
              (make-node 29 'b
                         (make-node
                          15
                          'c
                          (make-node 10 'd NONE NONE)
                          (make-node 24 'e NONE NONE))
                         NONE)
              (make-node 89 'f
                         (make-node 77 'g NONE NONE)
                         (make-node 95 'h NONE (make-node 99 'i NONE NONE)))))

; produces a sequence of ssns as they are read left to right
; BT -> [List-of Number]
(check-expect (inorder tree1) (list 15 24))
(check-expect (inorder bst1) (list 10 15 24 29 63 77 89 95 99)) 
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (append (inorder (node-left bt))
                  (list (node-ssn bt))
                  (inorder (node-right bt)))]))

; For a binary search tree inorder produces a list of numbers in ascending order

; returns the Symbol associated with a ssn (n) in a given BST
; or returns NONE if not found
(check-expect (search-bst NONE 74) NONE)
(check-expect (search-bst bst1 95) 'h)
(check-expect (search-bst bst1 94) NONE)
(define (search-bst bst n)
  (cond
    [(no-info? bst) NONE]
    [(= n (node-ssn bst)) (node-name bst)]
    [(< n (node-ssn bst)) (search-bst (node-left bst) n)]
    [(> n (node-ssn bst)) (search-bst (node-right bst) n)]))


