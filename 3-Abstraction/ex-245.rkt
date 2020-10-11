;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-245) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [Number -> Number] [Number -> Number] -> Boolean
; Determines if two functions return the same number
; when given 1.2, 3, and -5.775
(define (sf1 x) x)
(define (sf2 x) (- x 1))
(define (sf3 x) (+ (- x 1) 1))

(check-expect (function=at-1.2-3-and-5.775? sf1 sf3) #true)
(check-expect (function=at-1.2-3-and-5.775? sf2 sf1) #false)

(define (function=at-1.2-3-and-5.775? f1 f2)
  (and
   (= (f1 1.2) (f2 1.2))
   (= (f1 3) (f2 3))
   (= (f1 -5.775) (f2 -5.775))))

; No, I don't think we can hope to define a function=?,
; which determines whether two functions from numbers
; to numbers are equal because we would have to try an infinite
; number of input numbers to see if they get the same output.
; Some functions may output the same numbers in some range, but may
; output another number at a different range.
;
; For instance, given two functions
;
(define (inc1 x) (+ x 1))
(define (inc2 x)
  (cond
    [(< x 10000) (+ x 1)]
    [else (+ x 2)]))
; We could test a huge amount of numbers and get the same result,
; but if we reached a certain point, the numbers would change. We
; would need to have as many test cases as numbers available, which
; is inconceivable.

; The only way to be able to write such a function would be to
; have capabilities in the language that allow us to programattically
; inspect the contents of a function.
