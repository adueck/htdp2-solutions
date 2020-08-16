;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-233) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(check-expect (make-row '()) '())
(check-expect (make-row `(1 2))
              `((td "1") (td "2")))
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

; A Cell-Content is one of:
; - Number
; - String
; interpretation the inner contents of a td cell
 
; Cell-Content -> ... nested list ...
; creates a cell for an HTML table from Cell-Content
(check-expect (make-cell 5) `(td "5"))
(check-expect (make-cell "abc") `(td  "abc"))
(define (make-cell c)
  `(td ,(cond
          [(string? c) c]
          [(number? c) (number->string c)])))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers
(check-expect (make-table `(1 2) `(3 4 5))
              `(table ((border "1"))
                      (tr (td "1") (td "2"))
                      (tr (td "3") (td "4") (td "5"))))
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

; ... nested list ... -> ... nested list ...
; creates an HTML table from a list of lists of numbers
(check-expect (make-table.v2 '((1 2) (3 4 5)))
              `(table ((border "1"))
                      (tr (td "1") (td "2"))
                      (tr (td "3") (td "4") (td "5"))))
(define (make-table.v2 rows)
  `(table ((border "1"))
          ,@(make-rows rows)))

; ... nested list ... -> ... nested list ...
(check-expect (make-rows `((1 2) (3 4 5)))
`((tr (td "1") (td "2"))
  (tr (td "3") (td "4") (td "5"))))
(define (make-rows rows)
  (cond
    [(empty? rows) '()]
    [else `(
            (tr ,@(make-row (first rows)))
            ,@(make-rows (rest rows)))]))

(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; List-of-strings -> ... nested list ...
(check-expect (ranking '("a" "b" "c"))
              '((1 "a") (2 "b") (3 "c")))
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; List-of-strings -> ... nested list ...
; Adds a string version of a number for the rank of
; each string in a list
(check-expect (add-ranks '("a" "b" "c"))
              '((3 "a") (2 "b") (1 "c")))
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; List-of-strings -> ... nested list ...
; produces an HTML table from a list of ranked songs
(check-expect (make-ranking one-list)
              `(table ((border "1"))
                      (tr (td "1") (td "Asia: Heat of the Moment"))
                      (tr (td "2") (td "U2: One"))
                      (tr (td "3") (td "The White Stripes: Seven Nation Army"))))
(define (make-ranking los)
  (make-table.v2 (ranking los)))

(check-expect
 `(0 ,@'(1 2 3) 4)
 (list 0 1 2 3 4))

(check-expect
 `(("alan" ,(* 2 500))
  ("barb" 2000)
  (,@'("carl" " , the great")   1500)
  ("dawn" 2300))
 (list
  (list "alan" 1000)
  (list "barb" 2000)
  (list "carl" " , the great" 1500)
  (list "dawn" 2300)))

(check-expect
 `(html
   (body
     (table ((border "1"))
       (tr ((width "200"))
         ,@(make-row '( 1  2)))
       (tr ((width "200"))
         ,@(make-row '(99 65))))))
 (list 'html
  (list 'body
        (list 'table
              (list (list 'border "1"))
              (list 'tr (list (list 'width "200"))
                    (list 'td "1")
                    (list 'td "2"))
              (list 'tr (list (list 'width "200"))
                    (list 'td "99")
                    (list 'td "65"))))))
                    
