;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-257) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c))
              (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))
              (foldl / 1 '(6 3 2)))
(define (f*oldl f e l)
  (foldr f e (reverse l)))

; [X] Natural [Natural -> X] -> [List-of X]
; build-l*st works just like build-list
(check-expect (build-l*st 5 add1)
              (build-list 5 add1))
(check-expect (build-l*st 3 number->string)
              (build-list 3 number->string))
(check-expect (build-l*st 0 sub1)
              (build-list 0 sub1))
(define (build-l*st n f)
  (cond
    [(= n 0) '()]
    [else (add-at-end
           (f (sub1 n))
           (build-l*st (sub1 n) f))]))

; [List-of X] -> Posn
; adds X onto a [List-of X]
(check-expect (add-at-end 3 (list 2 4 5))
              (list 2 4 5 3))
(check-expect (add-at-end "hi" '())
              (list "hi"))
(check-expect (add-at-end "bar" (list "foo"))
              (list "foo" "bar"))
(define (add-at-end x lox)
  (cond
    [(empty? lox) (list x)]
    [else (cons (first lox)
                (add-at-end x (rest lox)))]))