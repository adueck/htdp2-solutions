;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-491) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute '(50 40 70 30 30))
              '(50 90 160 190 220))
(define (relative->absolute l)
  (cond
    [(empty? l) '()]
    [else (local ((define rest-of-l
                    (relative->absolute (rest l)))
                  (define adjusted
                    (map (lambda (p) (+ (first l) p)) rest-of-l)))
            (cons (first l) adjusted))]))

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
; but uses the foldr abstraction
(check-expect (relative->absoluteF '(50 40 70 30 30))
              '(50 90 160 190 220))
(define (relative->absoluteF l)
 (reverse
   (foldr (lambda (f l) (cons (+ f (first l)) l))
          (list (first l))
          (reverse (rest l)))))

; feed (list 3 2)

; on 2
; accum (list 1)
; (cons (+ 2 1) (list 1))
; (cons 3 (list 1))
; (list 3 1)

; on 3
; accum (list 3 1)
; (cons (+ 3 3) (list 3 1))
; (cons 6 (list 3 1))
; (list 6 3 1)

; Does this friend’s solution mean there is no need for our complicated design
; in this motivational section?

; No, there is still a need for an accumulator, in fact the foldr abstraction
; actually uses an accumulator (initialized by the base value).
; Wrapping the foldr abstraction in a function that doesn't take the base/
; starting value is essentially the same thing as wrapping relative->absolute/a
; in relative->absolute.v2 below

; [List-of Number] -> [List-of Number]
; converts a list of relative to absolute distances
; the first number represents the distance to the origin
 
(check-expect (relative->absolute.v2 '(50 40 70 30 30))
              '(50 90 160 190 220))
 
(define (relative->absolute.v2 l0)
  (local (
    ; [List-of Number] Number -> [List-of Number]
    (define (relative->absolute/a l accu-dist)
      (cond
        [(empty? l) '()]
        [else
          (local ((define accu (+ (first l) accu-dist)))
            (cons accu
                 (relative->absolute/a (rest l) accu)))])))
    (relative->absolute/a l0 0)))