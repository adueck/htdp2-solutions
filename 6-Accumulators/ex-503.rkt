;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-503) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-error (rotate '((0 4 5) (0 2 3))))
(define (rotate M)
  (cond
    [(not (= (first (first M)) 0)) M]
    [(andmap (lambda (x) (= 0 (first x))) M) (error "all rows start with 0")]
    [else
     (rotate (append (rest M) (list (first M))))]))

; Matrix -> Matrix 
; finds a row that doesn't start with 0 and
; uses it as the first one
; generative moves the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(check-expect (rotate.v2 '((0 4 5 7) (0 1 2 3) (2 3 4 5)))
              '((2 3 4 5) (0 4 5 7) (0 1 2 3)))
(check-error (rotate.v2 '((0 4 5) (0 2 3))))
(check-expect (rotate.v2 '()) '())
(define (rotate.v2 M0)
  (if
   (and
    (> (length M0) 0)
    (andmap (lambda (x) (= 0 (first x))) M0))
   (error "all rows start with 0")
   (local (; Matrix [List-of MatrixRow] -> Matrix 
           ; accumulator seen rows from M0 that will be put to the bottom
           (define (rotate/a M seen)
             (cond
               [(empty? M) seen]
               [else (if (= (first (first M)) 0)
                         (rotate/a (rest M) (cons (first M) seen))
                         (append M (reverse seen)))])))
     (rotate/a M0 '()))))

; Creates a sample matrix of n rows where
; all rows but the last have a leading co-efficient of 0
; Number -> Matrix
(check-expect (samp-matrix 3)
              (list
               (list 0 2 3 4 5 6)
               (list 0 2 3 4 5 6)
               (list 1 2 3 4 5 6)))
(define (samp-matrix n)
  (append
   (build-list (sub1 n) (lambda (x) (list 0 2 3 4 5 6)))
   (list (list 1 2 3 4 5 6))))

(define mat100 (samp-matrix 100))
(define mat1000 (samp-matrix 1000))
(define mat10000 (samp-matrix 10000))
(define mat100000 (samp-matrix 100000))
 