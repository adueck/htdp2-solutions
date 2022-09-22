;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-477) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a))
                              without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))
 
; [List-of [List-of 1String]] -> Boolean
; are the words "rat", "art", and "tar" members of the given list?
(define (all-words-from-rat? w)
  (and (member (explode "rat") w)
       (member (explode "art") w)
       (member (explode "tar") w)))
 
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)

; 1. Trivially solvable
;  - for each item, there has to be possibilites starting with that letter
; 2. How to solve the trivially solvable
;  a. get the solutions for the possibilitios without the letter
;  b. add the letter on to each of those solutions
; 3. How does this generate more easily sovlable problems?
;  - since we are removing a letter, we will have less combinations possible with the remaining letters.
;  - this is the same problem, but smaller (till it becomes a zero - one item list, which is easy)
; 4. The solution to 2 a. is the same as the original problem. All we need to do is add the item from 1. (each item) to
; the solutions we are accumulating with foldr

; For each of the letters picked by foldr, the algorithm will have to keep reaching back and seeing the possible
; combinations with less letters, until it reaches the end of the other letters - in which case it terminates with '(())
