;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-189-search-sorted) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number List-of-numbers -> Boolean
; checks to see if n exists in a sorted list of numbers
(check-expect (search-sorted 2 '()) #false)
(check-expect (search-sorted 2 (list 1 2 3)) #true)
(check-expect (search-sorted 2 (list 1 3 5)) #false)
(define (search-sorted n lon)
  (cond
    [(empty? lon) #false]
    [(cons? lon) (cond
                   [(< n (first lon)) #false] ; we can stop looking
                   [else (or
                          (= (first lon) n)
                          (search-in-sorted n (rest lon)))])]))