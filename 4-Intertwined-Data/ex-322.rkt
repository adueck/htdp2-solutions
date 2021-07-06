;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-322) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
 

; finds the name of the node whose value is n
; otherwise returns false
(check-expect (search-bt 24 tree1) 'i)
(check-expect (search-bt 29 tree1) #false)
(check-expect (search-bt 87 tree2) 'h)
(check-expect (search-bt 94 tree2) #false)
(define (search-bt n bt)
  (cond
    [(no-info? bt) #false]
    [else (search-node n bt)]))

(define (search-node n node)
  (if (= (node-ssn node) n) (node-name node)
      (local (
              (define left-res (search-bt n (node-left node)))
              (define right-res (search-bt n (node-right node))))
        
        (cond
        [(not (boolean? left-res)) left-res]
        [(not (boolean? right-res)) right-res]
        [else #false]))))
