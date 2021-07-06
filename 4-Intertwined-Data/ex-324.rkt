;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-324) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
              (make-node 29 'a
                         (make-node
                          15
                          'a
                          (make-node 10 'a NONE NONE)
                          (make-node 24 'a NONE NONE))
                         NONE)
              (make-node 89 'a
                         (make-node 77 'a NONE NONE)
                         (make-node 95 'a NONE (make-node 99 'a NONE NONE)))))

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


