;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-320) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; An S-expr is one of: 
; – Number
; - String
; - Symbol
; – SL
 
; An Los (List-of-S-exprs)
; – '()
; – (cons S-expr SL)

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count '(1 3 'a) 'a) 1)
(check-expect (count '("hi" a (a 3 (5 a))) 'a) 3)
(define (count sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [else (local (; Los Symbol -> N
                  ; Counts all the occurances of sy in los
                  (define (count-los los sy)
                    (foldr
                     (lambda (x tot) (+ (count x sy) tot))
                     0
                     los)))
           (count-los sexp sy))]))
