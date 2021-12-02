;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-394) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A NL is a [List-of Number] sorted in ascending order

; A ER is a structure
;  (make-er NL NL)
; giving the lists of matching and remaining numbers from a NL
(define-struct er [match rem])

; Produces a single set of sorted numbers containing all the
; numbers that occur both in nl1 and nl2
; NL NL -> NL
(check-expect (merge '(1 2 2 3 6) '(2 2 2 6 9))
              '(2 2 2 2 2 6 6))
(check-expect (merge '(1 3 3 4) '(3 5)) '(3 3 3))
(check-expect (merge '(2 4 6) '(3 5 7)) '())
(check-expect (merge '() '()) '())
(define (merge nl1 nl2)
  (cond
    [(empty? nl1) '()]
    [else (cond
            [(member (first nl1) nl2)
             (local ((define current (first nl1))
                     (define er1 (extract-matches current nl1))
                     (define er2 (extract-matches current nl2)))
               (append (er-match er1)
                       (er-match er2)
                       (merge (er-rem er1) (er-rem er2))))]
            [else (merge (rest nl1) nl2)])]))

; Finds all matches of a given n in a nl, returns a ER
; giving any matches and the remaining (non-matching) numbers in a nl
; Number NL -> ER
(check-expect (extract-matches 2 '(1 2 2 2 5))
              (make-er '(2 2 2) '(1 5)))
(check-expect (extract-matches 5 '(1 4 7 8 8))
              (make-er '() '(1 4 7 8 8)))
(define (extract-matches n nl)
  (make-er
   (filter (lambda (x) (= x n)) nl)
   (filter (lambda (x) (not (= x n))) nl)))