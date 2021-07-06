;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)

; Dex is on of:
; - File.v1
; - Dir.v1a

; A Dir.v1a (short for directory) is one of: 
; – [List of Dex]
 
; A File.v1 is a String.

(define dir1 '(
  ("part1" "part2" "part3")
  "read!"
  (
   ("hang" "draw")
   ("read!"))))

; Dir.v1 -> Number
; Determines how many files a directory contains
(check-expect (how-many '()) 0)
(check-expect (how-many dir1) 7)
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(string? (first dir))
     (+ 1
      (how-many (rest dir)))]
    [else (+
           (how-many (first dir))
           (how-many (rest dir)))]))

; Dir.v1a -> Number
; Determines how many files a directory contains
(check-expect (how-many-a '()) 0)
(check-expect (how-many-a dir1) 7)
(define (how-many-a dir)
  (foldl
   (lambda (x count) (+ count
                        (if (string? x)
                            1
                            (how-many-a x))))
   0
   dir))

