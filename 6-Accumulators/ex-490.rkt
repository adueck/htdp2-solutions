;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-490) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
                    (add-to-each (first l) rest-of-l)))
            (cons (first l) adjusted))]))
 
; Number [List-of Number] -> [List-of Number]
; adds n to each number on l
 
(check-expect (cons 50 (add-to-each 50 '(40 110 140 170)))
              '(50 90 160 190 220))
(define (add-to-each n l)
  (map (lambda (p) (+ n p)) l))

(check-expect (relative->absolute/a '(50 40 70 30 30) 0)
              '(50 90 160 190 220))
(define (relative->absolute/a l acc-dist)
  (cond
    [(empty? l) '()]
    [else (local
            ((define curr-dist (+ (first l) acc-dist)))
            (cons curr-dist
                  (relative->absolute/a (rest l) curr-dist)))]))

