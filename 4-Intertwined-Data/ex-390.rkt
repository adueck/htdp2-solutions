;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-390) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct branch [left right])
 
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)
(define ts1 'a)
(define ts2 (make-branch 'b (make-branch 'c 'd)))
(define ts3 (make-branch
             (make-branch 'ab 'cd)
             (make-branch (make-branch 'p 'o)
                          (make-branch 'x 'y))))
 
; A Direction is one of:
; – 'left
; – 'right
 
; A list of Directions is also called a path.

; TOS [List-of Directions] -> Symbol
; Picks a symbol from a given los by grabbing it with a lod
(check-expect (tree-pick ts1 '()) 'a)
(check-expect (tree-pick ts2 '(right right)) 'd)
(check-error (tree-pick ts2 '(left right left left)) "tree too short")
(check-error (tree-pick ts3 '(right)) "directions too short")
(check-expect (tree-pick ts3 '(right left left)) 'p) 
(define (tree-pick los lod)
  (cond
    [(and (empty? lod) (symbol? los)) los]
    [(and (empty? lod) (branch? los))
     (error "directions too short")]
    [(and (cons? lod) (symbol? los))
     (error "tree too short")]
    [(and (cons? lod) (branch? los))
     (if (symbol=? (first lod) 'right)
         (tree-pick (branch-right los) (rest lod))
         (tree-pick (branch-left los) (rest lod)))]))

; possible outcomes
;               (symbol? los)        (branch? los)
; (empty? lod)  
; (cons? lod)
