;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-451) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number]

(define table1 (make-table 3 (lambda (i) i)))

; N -> Number
(define (a2 i)
  (if (= i 0)
      pi
      (error "table2 is not defined for i =!= 0")))
 
(define table2 (make-table 1 a2))
(define table3 (make-table 10 (lambda (x) (- (* 2 x) 6))))
(define table4 (make-table 20 (lambda (x) (- (expt x 3) 1))))

; Table N -> Number
; looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i))

; Table -> Number
; finds the smallest index for a root of a table
; searches in a linear manner
(check-expect (find-linear table1) 0)
(check-error (find-linear table2))
(check-expect (find-linear table3) 3)
(check-expect (find-linear table4) 1)
(define (find-linear t)
  (local ((define (flin n)
            (cond
              [(> n (table-length t)) (error "reached end of table root not found")]
              [else (if (= 0 (table-ref t n))
                        n
                        (flin (add1 n)))])))
    (flin 0)))

; Number Number -> Number
; find the midpoint of two integers
(check-expect (get-mid 0 10) 5)
(check-expect (get-mid 0 0) 0)
(check-expect (get-mid 1 4) 2)
(define (get-mid a b)
  (local ((define t (+ b a)))
    (if (= 0 t)
        a
        (floor (/ t 2)))))

; Table Number Number -> Number
; finds the smallest index for a root of a table
; searches in a linear manner
(check-expect (find-binary table1) 0)
(check-error (find-binary table2))
(check-expect (find-binary table3) 3)
(check-expect (find-binary table4) 1)
(define (find-binary t)
  (local
    ((define (fb left right)
       (local ((define mid (get-mid left right))
               (define y@mid (table-ref t mid)))
         (cond
           [(= y@mid 0) mid]
           [(= mid left right) (error "couldn't find root")]
           [(< y@mid 0) (fb mid right)]
           [(> y@mid 0) (fb left mid)]))))
    (fb 0 (table-length t))))