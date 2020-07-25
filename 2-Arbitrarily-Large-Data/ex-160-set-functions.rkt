;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-160-set-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-string String -> N
; determines how often s occurs in los
(check-expect (count '() "foo") 0)
(check-expect (count (cons "foo" (cons "bar" '())) "foo")
              1)
(check-expect (count (cons "foo"
                           (cons "bar"
                                 (cons "foo" '())))
                     "foo")
              2)
(define (count los s)
  (cond
    [(empty? los) 0]
    [else
     (+
      (if (string=? (first los) s) 1 0)
      (count (rest los) s))]))

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)
; 
; Son is used when it 
; applies to Son.L and Son.R
  

; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s

; Son
(define es '())

; Number Son -> Boolean
; is x in s
(define (in? x s)
  (member? x s))

; Number Son.L -> Son.L
; removes x from s 
(define s1.L
  (cons 1 (cons 1 '())))
 
(check-expect
  (set-.L 1 s1.L) es)
 
(define (set-.L x s)
  (remove-all x s))
  

; Number Son.R -> Son.R
; removes x from s
(define s1.R
  (cons 1 '()))
 
(check-expect
  (set-.R 1 s1.R) es)
 
(define (set-.R x s)
  (remove x s))

; Number Son.L -> Son.L
; adds x to s
(define sl1 (cons 2 (cons 3 '())))
(define (contains5? s) (in? 5 s))
(check-satisfied (set+.L 5 sl1) contains5?)
(define (set+.L x s)
  (cons x s))

; Number Son.R -> Son.R
; adds x to s
(check-satisfied (set+.R 5 sl1) contains5?)
(define sr1 (cons 2 (cons 3 '())))
(check-member-of (set+.R 2 sr1)
                 (cons 2 (cons 3 '()))
                 (cons 3 (cons 2 '())))
(define (set+.R x s)
  (cond
    [(in? x s) s]
    [else (cons x s)]))
