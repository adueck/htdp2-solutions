;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-442) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (clever-sort (list 4 2 9 1)) (list 1 2 4 9))
(check-expect (clever-sort (list 4 2 3 1 8 7 6 5 9 11 13))
                           (list 1 2 3 4 5 6 7 8 9 11 13))
(check-expect (clever-sort '()) '())
(define (clever-sort alon)
  (if (<= (length alon) 10)
      (sort< alon)
      (quick-sort< alon)))
  

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort< (list 4 2 9 1)) (list 1 2 4 9))
(check-expect (quick-sort< '()) '())
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) (list (first alon))]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-expect (sort< (list 4 2 9 1)) (list 1 2 4 9))
(check-expect (sort< '()) '())
(define (sort< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort< (rest l)))]))

; [List-of Number] -> TestResult
(define (create-results t)
  (list
   "quick-sort<"
   (second (time (quick-sort< t)))))

(define rpt (build-list 1000 (lambda (x) 0)))

; Number -> [Number Number]
; Performance tests a random list of l length against both sort< and quick-sort<
; The sort is ran 1000 times for each function
; The results are printed to the console as a side-effect (sort<, then quick-sort<)
(define (make-tests l)
  (local
    ((define tlist (r-list l)))
    (map
     ; time running the function 1000 times
     (lambda (f) (first (first (time
                         (map (lambda (x) (f tlist)) rpt)))))
     (list sort< quick-sort<))))

; sort< is consistenly faster for lists of length 10 and below.
; (It varies a bit depending on the makeup of the list)

; Number -> [List-of Number]
; Creates a random list of length l
(define (r-list l)
  (build-list l (lambda (x) (random 1000))))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (<= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

 
; [List-of Number] Number -> [List-of Number]
(define (largers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (> (first alon) n)
              (cons (first alon) (largers (rest alon) n))
              (largers (rest alon) n))]))
 
; [List-of Number] Number -> [List-of Number]
(define (smallers alon n)
  (cond
    [(empty? alon) '()]
    [else (if (< (first alon) n)
              (cons (first alon) (smallers (rest alon) n))
              (smallers (rest alon) n))]))

