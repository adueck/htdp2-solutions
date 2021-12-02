;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-397) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A ER (Employee Record) is a structure
;  (make-er string number number)
(define-struct er [name no rate])

; A TC (Time Card) is a structure
;  (make-tc number numer)
(define-struct tc [no hours])

; A WR (Wage Record) is a structure
;  (make-wr string number)
(define-struct wr [name wage])

; [List-of ER] [List-of TC] -> [List-of WR]
(check-expect (wages*.v3
               (list (make-er "Bill" 1 5) (make-er "Frank" 2 3))
               (list (make-tc 2 20) (make-tc 1 25)))
               (list (make-wr "Bill" 125) (make-wr "Frank" 60)))
(check-expect (wages*.v3 '() '()) '())
(check-error (wages*.v3
               (list (make-er "Bill" 1 5) (make-er "Frank" 2 3))
               (list (make-tc 5 20) (make-tc 7 25))))
(check-error (wages*.v3
               (list (make-er "Bill" 1 5) (make-er "Frank" 2 3))
               (list (make-tc 3 20) (make-tc 1 25))))
(define (wages*.v3 ler ltc)
  (cond
    [(empty? ler) '()]
    [else (local ((define er (first ler))
                  (define tc (findf
                              (lambda (x) (= (er-no er) (tc-no x)))
                              ltc)))
            (if (false? tc)
                (error "record not found")
                (cons
                  (make-wr (er-name er) (* (er-rate er) (tc-hours tc)))
                  (wages*.v3 (rest ler) ltc))))]))

; [X -> Boolean] [List-of X] -> Maybe X
; Need to implement this myself because for some reason
; it's not finding it defined
; https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._findf%29%29
(check-expect (findf (lambda (x) (> x 5)) '(3 2 7 9)) 7)
(check-expect (findf (lambda (x) (string=? x "c")) '("a" "b")) #false)
(check-expect (findf (lambda (x) (= 25 (tc-no x)))
                     (list (make-tc 2 20) (make-tc 1 25)))
              #false)
(define (findf f l)
  (cond
    [(empty? l) #false]
    [else (if (f (first l))
              (first l)
              (findf f (rest l)))]))