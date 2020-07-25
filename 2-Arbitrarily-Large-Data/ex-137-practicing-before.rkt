;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex-137-practicing-before) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-strings is one of: 
; - ()'
; - (cons String List-of-strings)

; List-of-strings -> Number
; counts how many strings alos contains
(check-expect (how-many '()) 0)
(check-expect (how-many (cons "a" '())) 1)
(check-expect (how-many (cons "b" (cons "a" '()))) 2)
(define (how-many alos)
  (cond
    [(empty? alos) 0]
    [else (add1 (how-many (rest alos)))]))

; A List-of-numbers is one of: 
; - ()'
; - (cons Number List-of-strings)

; List-of-numbers -> Number
; adds the total of the list of numbers
(check-expect (total '()) 0)
(check-expect (total (cons 2 '())) 2)
(check-expect (total (cons 2 (cons 3 '()))) 5)
(define (total alon)
  (cond
    [(empty? alon) 0]
    [(cons? alon) (+
                   (first alon)
                   (total (rest alon)))]))

; List-of-numbers -> Boolean
; checks to see if it contains 3
(check-expect (contains-3 '()) #false)
(check-expect (contains-3 (cons 2 '())) #false)
(check-expect (contains-3 (cons 2 (cons 3 '()))) #true)
(define (contains-3 alon)
  (cond
    [(empty? alon) #false]
    [else (or
           (= (first alon) 3)
           (contains-3 (rest alon)))]))

; List-of-numbers Number -> Boolean
; checks to see if it contains n
(check-expect (containsn '() 1) #false)
(check-expect (containsn (cons 2 '()) 3) #false)
(check-expect (containsn (cons 2 (cons 3 '())) 3) #true)
(define (containsn alon n)
  (cond
    [(empty? alon) #false]
    [else (or
           (= (first alon) n)
           (contains-3 (rest alon)))]))

; List-of-numbers -> Number
; finds the smallest number in a list of at least one item
(check-expect (smallest (cons 2 '())) 2)
(check-expect (smallest (cons 2 (cons 3 '()))) 2)
(define (smallest alon)
  (cond
    [(empty? alon) 0]
    [else (if (= (smallest (rest alon)) 0)
               (first alon)
               (min (first alon)
                    (smallest (rest alon))))]))

; List-of-numbers -> Number
; finds the largest number in a list of numbers
(check-expect (largest '()) 0)
(check-expect (largest (cons 2 '())) 2)
(check-expect (largest (cons 2 (cons 3 '()))) 3)
(check-expect (largest (cons 1 (cons 5 (cons 2 '())))) 5)
(define (largest alon)
  (cond
    [(empty? alon) 0]
    [else (max
           (first alon)
           (largest (rest alon)))]))

(define-struct person [name age])
; Person is a structure
;  (make-person String Number)
; interpretation tho name and age of a person

; List-of-people is one of:
; - '()
; - (cons Person '())

; List-of-people -> Person
; find the oldest person in a list of people
(check-expect (oldest
               (cons (make-person "Fred" 20)
                     (cons (make-person "Bill" 40) '())))
              (make-person "Bill" 40))
(check-expect (oldest
               (cons (make-person "Bill" 40) '()))
              (make-person "Bill" 40))
(check-expect (oldest '())
              (make-person "No-one" 0))
(define (oldest alop)
  (cond
    [(empty? alop) (make-person "No-one" 0)]
    [else (cond
            [(>
              (person-age (first alop))
              (person-age (oldest (rest alop))))
             (first alop)]
            [else
             (oldest (rest alop))])]))

; map equivalent
; List-of-numbers -> List-of-numbers
; increment each number in a list by one
(check-expect (my-inc '()) '())
(check-expect (my-inc (cons 1 '())) (cons 2 '()))
(check-expect (my-inc (cons 2 (cons 3 '()))) (cons 3 (cons 4 '())))
(define (my-inc lon)
  (cond
    [(empty? lon) '()]
    [else
     (cons
      (add1 (first lon)) ; map function here
      (my-inc (rest lon)))]))

; filter equivalent
; List-of-numbers -> List-of-numbers
; filter out all values below 3
(check-expect (my-filt '()) '())
(check-expect (my-filt (cons 1 '())) '())
(check-expect (my-filt (cons 5 (cons 2 '()))) (cons 5 '()))
(define (my-filt lon)
  (cond
    [(empty? lon) '()]
    [else
     (if
       (>= (first lon) 3) ; filter predicate here
           (cons (first lon) (my-filt (rest lon)))
           (my-filt (rest lon)))]))




