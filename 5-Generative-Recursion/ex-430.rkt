;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-430) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort < '(4 1 0 9 2)) '(0 1 2 4 9))
(check-expect (quick-sort < '(5 2 2 1 7)) '(1 2 2 5 7))
(check-expect (quick-sort < '()) '())
(check-expect (quick-sort < '(4)) '(4))
(check-expect (quick-sort < '(1 8 2 3 4 10 1005 7)) '(1 2 3 4 7 8 10 1005))
(check-expect (quick-sort > '(1 8 2 3 4 10 1005 7)) '(1005 10 8 7 4 3 2 1))
(check-expect (quick-sort (lambda (x y) (> x 5)) '(1 10 2 15)) '(10 15 1 2))
(define (quick-sort f alon)
  (local ((define (partition alon n smaller others)
            (cond
              [(empty? alon) (list smaller others)]
              [(f (first alon) n) (partition
                                   (rest alon)
                                   n
                                   (cons (first alon) smaller)
                                   others)]
              [else (partition
                     (rest alon)
                     n
                     smaller
                     (cons (first alon) others))])))
    (cond
      [(empty? alon) '()]
      [(= (length alon) 1) alon]
      [else (local ((define pivot (first alon))
                    (define partitioned (partition (rest alon) pivot '() '()))
                    (define smallers (first partitioned))
                    (define others (second partitioned)))
              (append (quick-sort f smallers)
                      (list pivot)
                      (quick-sort f others)))])))
 
