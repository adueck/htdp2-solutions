;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-493) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] -> [List-of X]
; constructs the reverse of alox
 
(check-expect (invert '(a b c)) '(c b a))
 
(define (invert alox)
  (cond
    [(empty? alox) '()]
    [else
     (add-as-last (first alox) (invert (rest alox)))]))
 
; X [List-of X] -> [List-of X]
; adds an-x to the end of alox
 
(check-expect (add-as-last 'a '(c b)) '(c b a))
 
(define (add-as-last an-x alox)
  (cond
    [(empty? alox) (list an-x)]
    [else
     (cons (first alox) (add-as-last an-x (rest alox)))]))

; Every if n is the length of the list, every item must be
; processed n times. So the amount of processing grows exponentially
; as we add items to the list. O(n2)