;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex-437) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; [List-of X] -> Number
; Calculates the length of a List
(check-expect (general-l '(2 3 5)) 3)
(check-expect (general-l '()) 0)
(define (general-l P)
  (cond
    [(trivial? P) (solve-l P)]
    [else
     (combine-solutions-l
      P
      (general-l
       (generate P)))]))

; [List-of X] -> Number
; Calculates the length of a List
(check-expect (special-l '(2 3 5)) 3)
(check-expect (special-l '()) 0)
(define (special-l P)
  (cond
    [(empty? P) (solve-l P)]
    [else (combine-solutions-l
           P
           (special-l
            (rest P)))]))

; [List-of Number] -> [List-of Number]
; Negates a list of Numbers
(check-expect (special-n '(3 -7 1)) '(-3 7 -1))
(check-expect (special-n '()) '())
(define (special-n P)
  (cond
    [(empty? P) (solve-n P)]
    [else (combine-solutions-n
           P
           (special-n
            (rest P)))]))

; [List-of Number] -> [List-of Number]
; Negates a list of Numbers
(check-expect (general-n '(3 -7 1)) '(-3 7 -1))
(check-expect (general-n '()) '())
(define (general-n P)
  (cond
    [(trivial? P) (solve-n P)]
    [else (combine-solutions-n
           P
           (special-n
            (generate P)))]))

; [List-of String] -> [List-of String]
; Uppercases a list of Strings
(check-expect (special-u '("foo" "bar")) '("FOO" "BAR"))
(check-expect (special-u '()) '())
(define (special-u P)
  (cond
    [(empty? P) (solve-n P)]
    [else
     (combine-solutions-u
      P
      (special-u (rest P)))]))

; [List-of String] -> [List-of String]
; Uppercases a list of Strings
(check-expect (general-u '("foo" "bar")) '("FOO" "BAR"))
(check-expect (general-u '()) '())
(define (general-u P)
  (cond
    [(trivial? P) (solve-n P)]
    [else
     (combine-solutions-u
      P
      (special-u (generate P)))]))

; '() -> '()
(check-expect (solve-n '()) '())
(define (solve-n P) P)

; [List-of String] [List-of String] -> [List-of String]
(check-expect (combine-solutions-u '("foo" "baz") '("BAR"))
              '("FOO" "BAR"))
(define (combine-solutions-u notDoneL doneL)
  (cons (string-upcase (first notDoneL)) doneL))

; [List-of Number] [List-of Number] -> [List-of Number]
(check-expect (combine-solutions-n '(2 3 8) '(-4 -6))
             '(-2 -4 -6))
(define (combine-solutions-n notDoneL doneL)
  (cons (- (first notDoneL)) doneL))

; X Number -> Number
(check-expect (combine-solutions-l "a" 23) 24)
(check-expect (combine-solutions-l 8 2) 3)
(define (combine-solutions-l notDoneN doneN)
  (+ 1 doneN))

; '() -> 0
; Tells us that an empty list has a length of 0
(check-expect (solve-l '()) 0)
(define (solve-l P) 0)

; [List-of X] -> Boolean
(check-expect (trivial? '()) #t)
(check-expect (trivial? '(5)) #f)
(define (trivial? P)
  (empty? P))

; [List-of X] -> [List-of X]
; generate a smaller list for recursive combining
(check-expect (generate (list 2 4 7)) (list 4 7))
(define (generate P)
  (rest P))

; What do we learn from these exercises?

; In all three problems (computing a length of list, negating the numbers of a list,
; uppercasing a list) the generative recursion example is just an abstraction of the
; structural recursion, and the abstraction is the same each time, because each time
; we are working on processing structured data, one item/int at a time - so the GENERATIVE RESTRUCTION
; ABSTRACTION BECOMES UNNECESSARY / REDUNDAND

; Also the process for combining solutions and solving the trivial/empty condition of the
; structured is the same or abstractable for both problems using list - both could use a map extraction