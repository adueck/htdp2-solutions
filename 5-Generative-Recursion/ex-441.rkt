;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-441) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct
(check-expect (quick-sort '(
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) (first alon)]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))

; (quick-sort< (list 10 6 8 9 14 12 3 11 14 16 2))
;
; (append
;   (quick-sort< (list 6 8 9 3 2))
;   (list 10)
;   (quick-sort< (list 14 12 11 14 16))))
;
; (append
;   (append
;     (quick-sort< (list 3 2))
;     (list 6)
;     (quick-sort< (list 8 9)))
;   (list 10)
;   (append
;     (quick-sort< (list 12 11))
;     (list 14)
;     (quick-sort< (list 16))))
;
; (append
;   (append
;     (append
;       (quick-sort< (list 2))
;       (list 3)
;       (quick-sort< '()))
;     (list 6)
;     (append
;       (quick-sort< '())
;       (list 8)
;       (quick-sort< (list 9)))
;   (list 10)
;   (append
;     (append
;       (quick-sort< (list 11))
;       (list 12)
;       (quick-sort< '()))
;     (list 14)
;     (append
;       (quick-sort< '())
;       (list 16)
;       (quick-sort< '()))))
;
; (append
;   (append
;     (append
;       (append
;         (quick-sort< '())
;         (list 2)
;         (quick-sort< '()))
;       (list 3)
;       '())
;     (list 6)
;     (append
;       '()
;       (list 8)
;       (append
;         (quick-sort< '())
;         (list 9)
;         (quick-sort< '())
;   (list 10)
;   (append
;     (append
;       (append
;         (quick-sort< '())
;         (list 11)
;         (quick-sort< '()))
;       (list 12)
;       '())
;     (list 14)
;     (append
;       '()
;       (list 16)
;       '())))
;
; (append
;   (append
;     (list 2 3)
;     (list 6)
;     (list 8 9))
;   (list 10)
;   (append
;     (list 11 12)
;     (list 14)
;     (list 16)))
;
; (append
;   (list 2 3 6 8 9)
;   (list 10)
;   (list 11 12 14 16))
;
; (list 2 3 6 8 9 10 11 12 14 16)
;
; 20 recursive calls to quick-sort<
; 10 recursive calls to append

; (quick-sort< (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
;
; (append
;   (quick-sort< '())
;   (list 1)
;   (quick-sort< (list 2 3 4 5 6 7 8 9 10 11 12 13 14))
;
; (append
;   '()
;   (list 1)
;   (append
;     (quick-sort< '())
;     (list 2)
;     (quick-sort< 3 4 5 6 7 8 9 10 11 12 13 14)))
;
; (append
;   '()
;   (list 1)
;   (append
;     '()
;     (list 2)
;     (append
;       (quick-sort< '())
;       (list 3)
;       (quick-sort< 4 5 6 7 8 9 10 11 12 13 14))))
;  etc....
; leads to 24 recursive calls to quick-sort<
; and 14 recursive calls to append
;
; This doesn't contradict the first part of the exercise
; The append grow in a linear fashion 1 to 1 as the list to be sorted grows
; But the quick-sort< calls grow depending on how out of order the list is

 
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

