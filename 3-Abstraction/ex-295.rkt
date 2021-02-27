;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex-295) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generates n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))

; N -> [[List-of Posn] -> Boolean]
(define (n-inside-playground? n)
  (lambda (l)
    (and
     (= (length l) n)
     (local
       (; Posn -> Boolean
        (define (is-inside p)
          (and
           (<= (posn-x p) WIDTH)
           (<= (posn-y p) HEIGHT))))
       (andmap (lambda (p) (is-inside p)) l)))))

; N -> [List-of Posn]
; generates a few positions that are not random but fit in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns/bad 5)
                 (n-inside-playground? 5))
(define (random-posns/bad n)
  (build-list
   n
   (lambda (i)
     (make-posn 0 0))))