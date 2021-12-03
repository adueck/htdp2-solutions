;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-399) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [NEList-of X] -> X 
; returns a random item from the list
(check-random (random-pick '(2 3 4))
              (list-ref '(2 3 4) (random (length '(2 3 4)))))
(check-error (random-pick '()))
(define (random-pick l)
  (if (empty? l)
      (error "list must be at least one element long")
      (list-ref l (random (length l)))))

; [List-of String] [List-of [List-of String]] 
; -> 
; [List-of [List-of String]]
; produces the list of those lists in ll that do 
; not agree with names at any place
(check-expect (non-same (list "Bob" "Joe" "Frank")
                        (list
                         (list "Bob" "Frank" "Joe")
                         (list "Joe" "Bob" "Frank")
                         (list "Joe" "Frank" "Bob")
                         (list "Frank" "Bob" "Joe")))
              (list
               (list "Joe" "Frank" "Bob")
               (list "Frank" "Bob" "Joe")))
(define (non-same names ll)
  (local ((define (no-agreeance? x y)
            (cond
              [(empty? x) #true]
              [else (and
                     (not (string=? (first x) (first y)))
                     (no-agreeance? (rest x) (rest y)))])))
  (filter (lambda (a) (no-agreeance? a names)) ll)))
