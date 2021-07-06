;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-323) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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

; Determines whether a given number (n) occurs in
; a BinaryTree
; BT Number -> Boolean
(check-expect (contains-bt? tree1 24) #true)
(check-expect (contains-bt? tree1 85) #false)
(check-expect (contains-bt? tree2 15) #true)
(check-expect (contains-bt? NONE 23) #false)
(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #false]
    [else (or
           (= n (node-ssn bt))
           (contains-bt? (node-left bt) n)
           (contains-bt? (node-right bt) n))]))

; returns the Symbol associated with a ssn (n) in a given BT
; or returns #false if not found
(check-expect (search-bt NONE 45) #false)
(check-expect (search-bt tree1 24) 'i)
(check-expect (search-bt tree1 77) #false)
(define (search-bt bt n)
  (cond
    [(no-info? bt) #false]
    [else (if (= (node-ssn bt) n)
              (node-name bt)
              (local
                ((define left-res (search-bt (node-left bt) n))
                 (define right-res (search-bt (node-right bt) n)))
                (if (boolean? left-res) right-res left-res)))]))
