;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-506) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [X -> Y] [List-of X] -> [List-of Y]
; Applies proc to the elements of the lsts from the first elements to the last.
(check-expect (map.v2 add1 (list 1 2 3)) '(2 3 4))
(check-expect (map.v2 sub1 '()) '())
(define (map.v2 proc lst0)
  (local
    ; [X -> Y] [List-of X] [List-of Y] -> [List-of Y]
    ; accumulator acc is the list of elements that have
    ; been processed from the beginning of lst0
    ((define (map.v2/a proc lst acc)
       (cond
         [(empty? lst) (reverse acc)]
         [else (map.v2/a
                proc
                (rest lst)
                (cons (proc (first lst)) acc))])))
   (map.v2/a proc lst0 '())))