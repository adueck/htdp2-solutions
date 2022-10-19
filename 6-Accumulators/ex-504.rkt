;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-504) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> Number
; Consumes a list of digits and produces the correspoding number
(check-expect (to10 (list 1 0 2)) 102)
(check-expect (to10 (list 2)) 2)
(check-expect (to10 (list 5 2 0 0)) 5200)
(check-expect (to10 '()) 0)
(define (to10 lon)
  (local (; [List-of Number] Number -> Number
          ; accumulator a is the base to multiply
          ; each digit by
          (define (to10/a lon a)
            (cond
              [(empty? lon) 0]
              [else (+ (* (first lon) a)
                       (to10/a (rest lon) (/ a 10)))])))
    (to10/a lon (expt 10 (sub1 (length lon))))))


; [List-of Number] -> Number
; Consumes a list of digits and produces the correspoding number
(check-expect (to10.v2 (list 1 0 2)) 102)
(check-expect (to10.v2 (list 2)) 2)
(check-expect (to10.v2 (list 5 2 0 0)) 5200)
(check-expect (to10.v2 '()) 0)
(define (to10.v2 lon)
  (local (; [List-of Number] Number Number -> Number
          ; accumulator a is the amount of the digits to
          ; the left of lon
          (define (to10/a.v2 lon a)
            (cond
              [(empty? lon) a]
              [else (to10/a.v2 (rest lon)
                               (+ a (*
                                     (first lon)
                                     (expt 10 (sub1 (length lon))))))])))
    (to10/a.v2 lon 0)))
                       