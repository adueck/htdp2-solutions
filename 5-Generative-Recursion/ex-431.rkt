;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-431) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))
 
; [List-of X] N -> [List-of X]
; keeps the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))
 
; [List-of X] N -> [List-of X]
; removes the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))

; What is a trivially solvable problem?
;
; If we have an empty list, the bundled version will be an empty list

; How are trivial problems solved?
;
; returning '()

; How does the algorithm generate new problems that are more easily solvable
; than the original one? Is there one new problem that we generate or are there several?
;
; problem 1 - take the first chunk to be bundled
; problem 2 - remove the first chunk from the rest to be bundled

; Is the solution of the given problem the same as the solution of (one of) the
; new problems? Or, do we need to combine the solutions to create a solution for
; the original problem? And, if so, do we need anything from the original problem data?
;
; We need to combine the solutions no problem 1 and 2 by:
; - joining the letters and starting a list with it
; - continuing (recursively) to bundle the rest of the list
;   - to do this we will need the bundle size from the original problem data

; [List-of Number] -> [List-of Number]
; produces a sorted version of alon
; assume the numbers are all distinct 
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else (local ((define pivot (first alon)))
            (append (quick-sort< (smallers alon pivot))
                    (list pivot)
                    (quick-sort< (largers alon pivot))))]))
 
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

; What is a trivially solvable problem?
;
; - An empty list sorted with be an empty list
; - Finding an arbitrary pivot/center point for a list to sort around

; How are trivial problems solved?
;
; - returning '() when we get an empty list
; - picking the first item of the list as the arbitrary pivot point

; How does the algorithm generate new problems that are more easily solvable
; than the original one? Is there one new problem that we generate or are there several?
;
; The algorithm leaves us with the task of sorting the remaining numbers
; around the pivot point. This can be solved by dividing the numbers into
; numbers lower and higher than the pivot point and placing them on either
; side of the pivot point and sorting them again recursively


