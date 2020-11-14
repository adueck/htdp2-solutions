;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-273) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Z] [X -> Z] [List-of X] -> [List-of Z]
(check-expect (my-map (lambda (x) (+ x 3))
                      (list 2 4 5))
              (list 5 7 8))
(check-expect (my-map (lambda (x) (string-append "Hi " x))
                      (list "bill" "frank"))
              (list "Hi bill" "Hi frank"))
(define (my-map f li)
  (local (;[X Z] X Z -> Z
          (define (apply-f x z)
            (cons (f x) z)))
  (foldr apply-f '() li)))

